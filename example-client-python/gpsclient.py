#!/usr/bin/python
"""
gpsclient - this program takes GPS location fixes using a 
Sierra Wireless GPS enabled modem and posts the GPS lat and long to a server

Before running the script please change the team name listed in the global variables.

Once your team has set up their own server the script Json posts can be redirected 
to this sever by changing the positionURL within the Global variables.

By default the client script is using the (telstra.extranet) APN, 
this will mean that a publicly addressable IP is assigned to the device. 
While the IP is public the IP address is not statically assigned and can change 
between reconnections. Teams may also wish to use the (telstra.m2m) APN which 
assigns a private IP.

This script is provided as an example building block only to show what can be done 
and to give teams a basic client-server platform to work from. 
Please feel free to change, modify, rebuild or completely redesign components as 
specifically suits your teams chosen M2M objective.

If your team finds bugs in this baseline script or identifies better ways to 
implement the baseline code that is not specific to your solution, then please submit 
modifications via github. Teams will be recognised for their contributions in 
improving the baseline code.

Note: While we have used the GPS functionality of the modem in this baseline platform, 
teams do not have to use the GPS location functionality in their M2M solution.

Note: The script here is designed based around a remote device collecting data and 
sending it to a server for processing and display. However remember that M2M 
solutions are not limited to just this structure and can also include servers 
pushing information to remote devices, devices communicating directly with other 
devices or a combination of these structures.

Note: As the client program sends and receives serial AT data from the modem it 
should not be used while any other serial AT program (e.g. minicom) is connected as 
this will result in communication conflicts.

"""


##########
# IMPORT #
##########

import json
import Queue
import random
import re
import serial
import subprocess
import sys
import threading
import time
import traceback
import urllib2
from cookielib import CookieJar


####################
# Global Variables #
####################

startTime = 0
backoff = 5 # Sets the starting backoff time for attempting to reconnect (seconds)
backofftimer = backoff  # IPbackoff is doubled every time the connection fails. See connectbackoff()
backoffStart = time.time()
teamname = "Team ??"  # Change displayString value to reflect your team name
positionUrl = "http://example.innovation.telstra.com/api/position"  # Target server for JSON upload
setAPN = 'AT+CGDCONT=1,\"IP\",\"telstra.extranet\"' # To get Public IP
#setAPN = 'AT+CGDCONT=1,\"IP\",\"telstra.m2m\"' # To get Private IP
connectdictionary = {
    'DHCP_Release': {
        'type': 'OS',
        'printobjective': '[ -- ]    DHCP Release',
        'command': 'sudo dhclient -r wwan0',
        'matchcommand': ' ',
        'response': 'Cannot find device',
        'waitTime': 2,
        'printgood': ' ',
        'printbad': '[FAIL]    Error: DHCP release request failed',
        'value': ' ',
        'NextConnectState': 'ModemPowerOn',
        'sleepTime': 1
        },
    'ModemPowerOn': {
        'type': 'AT',
        'printobjective': ' ', 
        'command': 'AT+CFUN=1', 
        'matchcommand': 'CFUN', 
        'response': 'OK', 
        'waitTime': 2, 
        'printgood': '[ OK ]    Modem radio powering up',
        'printbad': '[FAIL]    Unable to power up modem radio',
        'value': ' ', 
        'NextConnectState': 'ModemPdpClose', 
        'sleepTime': 10
        }, 
    'ModemPdpClose': {
        'type': 'AT',
        'printobjective': ' ',
        'command': 'AT!SCACT=0,1',
        'matchcommand': 'SCACT',
        'response': 'OK',
        'waitTime': 2,
        'printgood': '[ OK ]    PDP connection closed',
        'printbad': '[FAIL]    Error: Unable to close PDP connection',
        'value': ' ',
        'NextConnectState': 'setAPN',
        'sleepTime': 1
        }, 
    'setAPN': {
        'type': 'AT',
        'printobjective': ' ',
        'command': setAPN,
        'matchcommand': 'CGDCONT',
        'response': 'OK',
        'waitTime': 2,
        'printgood': '[ OK ]    APN in profile 1 set:',
        'printbad': '[FAIL]    Error: Unable to set APN in profile 1',
        'value': setAPN,
        'NextConnectState': 'setDefaultAPN',
        'sleepTime': 1
        }, 
    'setDefaultAPN': {
        'type': 'AT',
        'printobjective': ' ',
        'command': 'AT!SCDFTPROF=1',
        'matchcommand': 'SCDFTPROF',
        'response': 'OK',
        'waitTime': 2,
        'printgood': '[ OK ]    Profile 1 set as default',
        'printbad': '[FAIL]    Error: Unable to set Profile 1 as default',
        'value': ' ',
        'NextConnectState': 'setProf1Manual',
        'sleepTime': 1
        }, 
    'setProf1Manual': {
        'type': 'AT',
        'printobjective': ' ',
        'command': 'AT!SCPROF=1,"m2mChallenge",0,0,0,0',
        'matchcommand': 'SCPROF',
        'response': 'OK',
        'waitTime': 2,
        'printgood': '[ OK ]    Profile 1 set as manual connection',
        'printbad': '[FAIL]    Error: Unable to set Profile 1 as manual connection',
        'value': ' ',
        'NextConnectState': 'ModemPdpOpen',
        'sleepTime': 1
        }, 
    'ModemPdpOpen': {
        'type': 'AT',
        'printobjective': '[ -- ]    Activating PDP connection using profile 1',
        'command': 'AT!SCACT=1,1',
        'matchcommand': 'SCACT',
        'response': 'OK',
        'waitTime': 10,
        'printgood': '[ OK ]    PDP connection on profile 1 activated',
        'printbad': '[FAIL]    Error: Unable to activate PDP connection using profile 1',
        'value': ' ',
        'NextConnectState': 'DHCP_Renew',
        'sleepTime': 4
        }, 
    'DHCP_Renew': {
        'type': 'OS',
        'printobjective': '[ -- ]    DHCP Renew',
        'command': 'sudo dhclient -nw wwan0',
        'matchcommand': '',
        'response': 'Cannot find device',
        'waitTime': 1,
        'printgood': ' ',
        'printbad': '[FAIL]    Error: DHCP renew request failed',
        'value': ' ',
        'NextConnectState': 'readIPwwan0',
        'sleepTime': 1
        }, 
    'readIPwwan0': {
        'type': 'OS',
        'printobjective': ' ',
        'command': '/sbin/ifconfig wwan0',
        'matchcommand': '',
        'response': '(?:[\d]{1,3})\.(?:[\d]{1,3})\.(?:[\d]{1,3})\.(?:[\d]{1,3})',
        'waitTime': 1,
        'printgood': '[ OK ] Successfully obtained IP address on wwan0',
        'printbad': '[FAIL] Could not obtain IP address on wwan0',
        'value': ' ',
        'NextConnectState': 'none',
        'sleepTime': 1
        }
    }

statedictionary = {
    'SetEcho': {
        'command': 'ATE1',
        'matchcommand': 'ATE1', 
        'response': 'OK', 
        'waitTime': 2, 
        'printgood': '[ OK ] Modem Echo On', 
        'printbad': '[FAIL] Modem Echo Activation Failed', 
        'value': ' ', 
        'nextstate': 'GetIMSI'
        },
    'GetIMSI': {
        'command': 'AT+CIMI', 
        'matchcommand': 'CIMI',
        'response': '(\d+)', 
        'waitTime': 2,
        'printgood': '[ OK ] SIM Card IMSI is',
        'printbad': '[FAIL] SIM Card IMSI Retrieval Failed',
        'value': 0, 
        'nextstate': 'GetIMEI'
        },
    'GetIMEI': {
        'command': 'AT+CGSN',
        'matchcommand': 'CGSN', 
        'response': '(\\d+)', 
        'waitTime': 2,
        'printgood': '[ OK ] Modem IMEI is',
        'printbad': '[FAIL] Modem IMEI Retrieval Failed',
        'value': 0, 
        'nextstate': 'GetCPUid'
        },
    'GetCPUid': {
        'command': ' ', 
        'matchcommand': ' ', 
        'response': 'Serial\s+\:(.*)$', 
        'waitTime': 2,
        'printgood': '[ OK ] Pi CPU Serial ID is:',
        'printbad': '[FAIL] Unable to get CPU Serial ID',
        'value': 0, 
        'nextstate': 'GetGPSstatus'
        },
    'GetGPSstatus': {
        'command': 'AT!GPSSTATUS?', 
        'matchcommand': 'GPSSTATUS', 
        'response': 'Fix Session Status = ACTIVE', 
        'waitTime': 1,
        'printgood': '[ OK ] Fix Session Status = ACTIVE',
        'printbad': '[ -- ] GPS tracking session not started', 
        'value': ' ', 
        'nextstate': 'GetGPSloc'
        },
    'StartTrack': {
        'command': 'AT!GPSTRACK=1,255,100,600,1', 
        'matchcommand': 'GPSTRACK', 
        'response': 'OK', 
        'waitTime': 1,
        'printgood': '[ OK ] Started GPS Tracking Session',
        'printbad': ' ', 
        'value': ' ', 
        'nextstate': 'GetGPSstatus'
        },
    'GetGPSloc': {
        'command': 'AT!GPSLOC?', 
        'matchcommand': 'GPSLOC', 
        'response': 'Lat', 
        'waitTime': 1,
        'printgood': ' ',
        'printbad': '[ -- ] Unable to retrieve GPS Lat and Long - make sure device has clear view of the sky', 
        'value': ' ', 
        'nextstate': 'GetGPSinfo'
        },
    'GetGPSinfo': {
        'command': 'AT!GPSSATINFO?', 
        'matchcommand': 'GPSSATINFO', 
        'response': 'Satellites in view', 
        'waitTime': 1,
        'printgood': ' ',
        'printbad': '[ -- ] NO SAT INFO', 
        'value': ' ', 
        'nextstate': 'GetGPSstatus'
        }
    }


##########################################################
# Code to manage connection and communication with modem #
##########################################################

def ModemDetect():
    """Checks if wireless modem is plugged into USB port and detected by system"""
    #print "[ -- ] Checking Modem Present"
    commandoutput = subprocess.Popen(["/sbin/ifconfig", "wwan0"], stdout=subprocess.PIPE)
    commandresult = commandoutput.communicate()[0]
    modeminterface = re.search('wwan0', commandresult) # If modem plugged in and detected there should be a wwan0 interface created
    if modeminterface:
        result = True  # wwan0 interface detected so assume modem is plugged in and correctly configured
    else:
        print "[FAIL] USB Modem interface is NOT detected"
        time.sleep(2)
        result = False
    return result


def sendThread(serialDevice, commandQueue):
    """Send AT to modem by placing in commandQueue"""
    while True:
        # Block to avoid a busy-wait.
        serialDevice.write(commandQueue.get(block=True))


def receiveThread(serialDevice, responseQueue):
    """Read AT response from modem by checking responseQueue"""
    lineBuffer = ""
    lineGroup = []
    nextChar = None
    char_recieved = False
    while True:
        nextChar = serialDevice.read()
        lineBuffer += nextChar
        if nextChar:
            # If data comes in then set timer alarm to send to response queue even if end of line is not received.
            char_recieved = True
            last_char_recieved = time.time()
            # Wait 2 seconds after last recieved modem char before raising timeout alarm
        if nextChar == '\n':   # Assume \r\n line termination
            lineGroup.append(lineBuffer)
            lineBuffer = ""
            if (''.join(lineGroup[-1]) == 'ERROR\r\n') or (''.join(lineGroup[-1]) == 'OK\r\n') or (''.join(lineGroup[-1]) == '+CME ERROR: no network service\r\n'):
                responseQueue.put(''.join(lineGroup), block=True)
                del lineGroup[:]
                char_recieved = False
                last_char_recieved = time.time()
        if char_recieved and (time.time() - last_char_recieved) > 2: 
            # Push data to responseQueue if last recieved char was more than more than 2 seconds ago
            #print "[ -- ] receiveThread timeout occured so put what has been recieved into the responseQueue and reset receiveThread"
            responseQueue.put(''.join(lineGroup), block=True)
            del lineGroup[:]
            char_recieved = False
            last_char_recieved = time.time()


############################################
# Code to manage wireless connection state #
############################################

def connectStateCommand(connectState, connectstarttime, commandQueue):
    """Based on current wireless connection state send appropriate AT command."""
    #print "[ -- ] Entering connectStateCommand in connection state:", connectState
    connectwaittime = time.time() - connectstarttime
    if connectwaittime > connectdictionary[connectState]["waitTime"]: # Throttle how often command can be sent
        if connectdictionary[connectState]["type"] == 'AT':
            if not connectdictionary[connectState]['printobjective'] == ' ': 
                print connectdictionary[connectState]['printobjective']
            command = connectdictionary[connectState]["command"]
            #print "[ -- ] Sending AT command", command
            commandQueue.put(command + "\r\n", block=True, timeout=1)
            #print "[ -- ] Wait x seconds after sending command"
            time.sleep(connectdictionary[connectState]["sleepTime"]) # Wait x seconds after sending command
            connectstarttime = time.time()
    return connectstarttime


def connectStateNext(connectState, connectstarttime, responseQueue):
    """
    Process AT responses based on the current wireless connection state 
    and chooses whether to change connection state based on the response.
    Also based on connection state it may send out commands to the Pi operating 
    system and processes response to determine next connect state.
    """
    #print "[ -- ] Entering connectStateNext in connection state:", connectState
    newconnectState = connectState
    try:
        if connectdictionary[connectState]["type"] == 'AT':
            r = responseQueue.get(block=True, timeout=1)  #Read response queue (if queue is empty this will cause an exception)
            matchcommand =  re.search(connectdictionary[connectState]['matchcommand'], r)
            response = re.search(connectdictionary[connectState]['response'], r)
            if matchcommand and response:
                if not connectdictionary[connectState]['printgood'] == ' ':
                    print connectdictionary[connectState]["printgood"], connectdictionary[connectState]["value"]
                newconnectState = connectdictionary[connectState]['NextConnectState']
                connectstarttime = 0  # State changed so don't wait to send next command
            elif matchcommand:
                if not connectdictionary[connectState]['printbad'] == ' ': 
                    print connectdictionary[connectState]["printbad"]
                if (connectState == 'ModemPdpOpen') and not (connectstarttime == 0):
                    newconnectState = connectdictionary[connectState]['NextConnectState']
                    connectstarttime = 0  # State changed so don't wait to send next command
        else:
            #print "Sending OS Command"
            if not connectdictionary[connectState]['printobjective'] == ' ': 
                print connectdictionary[connectState]['printobjective']
            command = connectdictionary[connectState]["command"]
            commandoutput = subprocess.Popen([command], shell=True, stdout=subprocess.PIPE)
            commandoutput.communicate()
            errcode = commandoutput.returncode
            if errcode == 0:
                if not connectdictionary[connectState]['printgood'] == ' ':
                    print connectdictionary[connectState]["printgood"]
            else:
                if not connectdictionary[connectState]['printbad'] == ' ':
                    print connectdictionary[connectState]["printbad"]
            if (connectState == 'DHCP_Release') or (connectState == 'DHCP_Renew'):
                time.sleep(connectdictionary[connectState]["sleepTime"])
                newconnectState = connectdictionary[connectState]['NextConnectState']
                connectstarttime = 0  # State changed so don't wait to send next command
        return newconnectState, connectstarttime
    except Queue.Empty:
        return newconnectState, connectstarttime


def ModemPdpConnect(commandQueue, responseQueue):
    """Initiate new PDP connection and obtain IP address on wwan0"""
    global backoffStart
    global backofftimer
    print "[ -- ] Running PDP Connection Commands"
    connectState = 'DHCP_Release'
    connectstarttime = time.time()
    while not connectState == 'readIPwwan0':
        connectState, connectstarttime = connectStateNext(connectState, connectstarttime, responseQueue)
        connectstarttime = connectStateCommand(connectState, connectstarttime, commandQueue)
    t = 3 # Try 3 times to read IP from wwan0 then give up
    while not t == 0:
        time.sleep(2)
        command = connectdictionary[connectState]["command"]
        commandoutput = subprocess.Popen([command], shell=True, stdout=subprocess.PIPE)
        commandresult = commandoutput.communicate()[0]
        if commandresult:
            wwanip = re.search(connectdictionary[connectState]['response'], commandresult)
            if wwanip:
                print "[ OK ] Successfully obtained IP address on wwan0"
                ModemIPchange(wwanip.group(0))
                t = 0
                connected = True
                backofftimer = backoff  # Got connection so reset backoff to original
            else:
                print "[FAIL] Could not obtain IP address on wwan0"
                t = t -1
                connected = False
        else:
            print "[FAIL] No response to ifconfig for wwan0"
            t = t -1
            connected = False
    backoffStart = time.time() # Measure backoff time from this last attempt
    return connected


def ModemIPcheck(commandQueue, responseQueue):
    """
    Checks if IP address assigned on the wireless modem interface wwan0
    If no IP then calls on ModemPdpConnect() to re-establish connection
    """
    if connectbackoff():
        connectStateNext('DHCP_Release', 0, responseQueue)
        connectStateNext('DHCP_Renew', 0, responseQueue)
        command = connectdictionary['readIPwwan0']["command"]
        commandoutput = subprocess.Popen([command], shell=True, stdout=subprocess.PIPE)
        commandresult = commandoutput.communicate()[0]
        wwanip = re.search(connectdictionary['readIPwwan0']['response'], commandresult)
        if wwanip:
            print "[ OK ] Pi wwan0 already connected with IP address", wwanip.group(0)
            result =  True
        else:
            print "[ -- ] Pi wwan0 NOT connected attempting to re-establish now"
            result = ModemPdpConnect(commandQueue, responseQueue)
    else:
        result = False
    return result


def ModemIPchange(wwanip):
    """
    Checks if modem IP has changed from the last acquired modem IP
    Saves current IP in file .current_ip for later reference
    """
    #print "[info] Checking if Wwan0 IP Changed"
    lastIP = subprocess.Popen("cat  ~/.current_ip", shell=True, stdout=subprocess.PIPE)
    lastIP = lastIP.communicate()[0]
    if lastIP:
        lastIPclean = re.search(connectdictionary['readIPwwan0']['response'], lastIP)
        if lastIP:
            if wwanip != lastIPclean.group(0):
                print "[ -- ] New wwan0 IP address detected ", wwanip, "was ", lastIPclean.group(0)
                iplist = "echo \"" + wwanip + "\" >|~/.current_ip"
                subprocess.Popen(iplist, shell=True, stdout=subprocess.PIPE) # Save IP address in file .current_ip
                # Place code here if you want device to do something in response to IP change. Example send email.
            else:
                print "[ -- ] Wwan0 IP address same as last acquired address", wwanip
        else:
            # File .current_ip did not exist or was empty
            print "[ -- ] New wwan0 IP address", wwanip
            iplist = "echo \"" + wwanip + "\" >|~/.current_ip"
            subprocess.Popen(iplist, shell=True, stdout=subprocess.PIPE) # Save IP address in file .current_ip


def connectbackoff():
    """If failure to connect on wireless this method sets a growing backoff period before attempting again"""
    global backofftimer
    timepassed = time.time() - backoffStart
    if timepassed > backofftimer:
        #print "[ -- ] Backoff timer has expired so ok to try to connect"
        backofftimer = backofftimer + backofftimer + random.random()  # Increase time before trying again
        if (backofftimer > 3600): # Set max backoff timer at 1 hour
            backofftimer = 3600 + random.randint(0, 300) # Backoff 1 hour with 5 minute random window
        return True
    else:
        #print "[ -- ] Backoff timer has not expired so cannot try to connect yet"
        timeremain = backofftimer - timepassed
        print "[ -- ] System can attempt reconnect in", timeremain, "seconds"
        return False


##################################################################
# Code to extract and format GPS information from modem response #
##################################################################

def getLongitude(r):
    """Extract longitude from response and format for transmission to server"""
    z =  re.search('Lon: (\d+) Deg (\d+) Min ([\.\d]+) Sec (\w)', r)
    if not z:
        return None
    else:
        # Now convert.
        degrees = int(z.group(1))
        minutes = int(z.group(2))
        seconds = float(z.group(3))
        primeMeridian = str(z.group(4))
        if (primeMeridian == 'E'):
            return degrees + ( ( minutes + (seconds / 60) ) / 60)
        elif (primeMeridian == 'W'):
            return (degrees + ( ( minutes + (seconds / 60) ) / 60))*-1
        else:
            raise Exception('Failure to parse longitude')


def getLatitude(r):
    """Extract latitude from response and format for transmission to server"""
    z =  re.search('Lat: (\d+) Deg (\d+) Min ([\.\d]+) Sec (\w)', r)
    if not z:
        return None
    else:
        # Now convert.
        degrees = int(z.group(1))
        minutes = int(z.group(2))
        seconds = float(z.group(3))
        hemisphere = str(z.group(4))
        # FIX bearing.
        if (hemisphere == 'N'):
            return degrees + ( ( minutes + (seconds / 60) ) / 60)
        elif (hemisphere == 'S'):
            return (degrees + ( ( minutes + (seconds / 60) ) / 60))*-1
        else:
            raise Exception('Failure to parse latitude')


def getTimeUTC(r):
    """Extract time from GPSLOC response and format for transmission to server"""
    #print "Extracting time from GPSLOC response"
    # Example Time: 2013 04 16 1 03:15:16 (GPS)
    z =  re.search('Time: (\d+) (\d+) (\d+) (\d+) ([\:\:\d]+)', r)
    if not z:
        return None
    else:
        # Now convert.
        yearUTC = z.group(1)
        monthUTC = z.group(2)
        dayUTC = z.group(3)
        timeUTC = z.group(5)
        timedateUTC = str(yearUTC) + "-" + str(monthUTC)  + "-" + str(dayUTC) + " " + str(timeUTC)
        # Example timedateUTC = 2013-04-16 03:15:16
        return timedateUTC


#####################################################
# Code to handle uploading collected data to server #
#####################################################

def postJson(targetUrl, latitude, longitude, timedateUTC, imei, imsi, cpuID, displayString):
    """Post data to server using JSON"""
    cookieJar = CookieJar()
    arbitraryText = "UTC Timestamp is " + str(timedateUTC)
    print " displayString " + str(displayString)  + " arbitraryText " + str(arbitraryText)
    print "Posting GPS coordinates lat: " + str(latitude) + " and long:" + str(longitude)
    print " imei:" + str(imei) + " imsi:" + str(imsi) + " cpuID:" + str(cpuID)
    p = json.dumps({"latitude": str(latitude), "longitude": str(longitude), "timestampUTC": str(timedateUTC),
                    "imei": str(imei), "imsi": str(imsi), "cpuID": str(cpuID),
                    "displayString": str(displayString), "arbitraryText": str(arbitraryText)})
    o2 = urllib2.build_opener(urllib2.HTTPCookieProcessor(cookieJar))
    o2.addheaders = [ ('Content-Type', 'application/json')]
    resp2  = o2.open(targetUrl, p)
    print resp2.info()
    return resp2.getcode()


###############################################################
# Code to manage state machine for getting GPS and modem data #
###############################################################

def stateCommand(currentState, statetrans, commandQueue):
    """Based on current state send appropriate AT command."""
    #print "[ -- ] Entering stateCommand in state:", currentState
    global startTime
    if statetrans and not (currentState == "GetGPSloc"):
        waitTime = 1000
    else:
        waitTime = time.time() - startTime
    if waitTime > statedictionary[currentState]["waitTime"]:
        command = statedictionary[currentState]["command"]
    else:
        command = ' '
        return False
    if not command == ' ':
        #print "[ -- ] Sending AT command", command
        commandQueue.put(command + "\r\n", block=True, timeout=1)
        startTime = time.time()
        return True


def getNEXTstate(currentState, responseQueue):
    """Based on current state and received response decided whether to change state"""
    newState = currentState
    StateChange = False
    try:
        if currentState == "GetCPUid":
            newState, StateChange = GetCPUid(currentState)
        else:
            r = responseQueue.get(block=True, timeout=1)  #Read response queue (if queue is empty this will cause an exception)
            matchcommand =  re.search(statedictionary[currentState]['matchcommand'], r)
            response = re.search(statedictionary[currentState]['response'], r)
            if matchcommand and response:
                if currentState == "GetIMSI" or currentState == "GetIMEI":
                    statedictionary[currentState]['value'] = response.group(0)
                elif currentState == "GetGPSloc":
                    latitude = getLatitude(r)
                    longitude = getLongitude(r)
                    timedateUTC = getTimeUTC(r)
                    if latitude and longitude:
                        imei = statedictionary['GetIMEI']['value']
                        imsi = statedictionary["GetIMSI"]['value']
                        cpuID = statedictionary["GetCPUid"]['value']
                        result = postJson(positionUrl, latitude, longitude, timedateUTC, imei, imsi, cpuID, teamname)
                        print result
                    else:
                        print "Unable to extract latitude or longitude from modem response"
                elif currentState == "GetGPSinfo":
                    statedictionary[currentState]['value'] = r
                print statedictionary[currentState]["printgood"], statedictionary[currentState]['value']
                newState = statedictionary[currentState]['nextstate']
                StateChange = True
            elif matchcommand:
                if currentState == "GetGPSstatus":
                    newState = "StartTrack" #GPS tracking session not started so go to StartTrack state instead of GetGPSloc
                    StateChange = True
                elif currentState == "GetGPSloc": 
                    #print "Posting test location Uluru -25.345 and 131.035 using time 2013-01-01 00:00:00"
                    #imei = statedictionary['GetIMEI']['value']
                    #imsi = statedictionary["GetIMSI"]['value']
                    #cpuID = statedictionary["GetCPUid"]['value']
                    #testpostresult = postJson(positionUrl, -25.345, 131.035, '2013-01-01 00:00:00' imei, imsi, cpuID, teamname) # Test Command Only
                    #print testpostresult
                    newState = statedictionary[currentState]['nextstate']
                    StateChange = True
                elif currentState ==  "StartTrack" or currentState == "GetGPSinfo":
                    newState = statedictionary[currentState]['nextstate']
                    StateChange = True
                print statedictionary[currentState]['printbad']
            else:
                # AT response out of sync with command sent, so discard response and stay in current state
                StateChange = False
        return newState, StateChange
    except Queue.Empty:
        #print "Empty Queue exception in state:", currentState
        return newState, StateChange


def GetCPUid(currentState):
    """Queries Raspberry Pi for its unique CPU ID"""
    commandoutput = subprocess.Popen(["/bin/cat", "/proc/cpuinfo"], stdout=subprocess.PIPE)
    commandresult = commandoutput.communicate()[0]
    z =  re.search(statedictionary[currentState]['response'], commandresult, re.MULTILINE)
    if z:
        statedictionary[currentState]['value'] = z.group(1)
        print statedictionary[currentState]["printgood"], statedictionary[currentState]['value']
        newState = statedictionary[currentState]['nextstate']
        StateChange = True
    else:
        print statedictionary[currentState]["printbad"]
        newState = currentState
        StateChange = False
    return newState, StateChange


#############################################
# Main program loop with exception handlers #
#############################################

def main():
    """gpsclient main. Includes initial system initialisation and exception handling"""
    print "[ -- ] System Initiating Setup"
    global startTime
    threadscreated = False
    setupmode = True
    while True:
        try:
            if (threadscreated == False) and ModemDetect():
                print "[ -- ] Creating Queue Threads"
                commandQueue = Queue.Queue()
                responseQueue = Queue.Queue()
                serialDevice = serial.Serial(port='/dev/ttyUSB3', timeout=1)
                serialDevice.open()
                st = threading.Thread(target=sendThread, args=(serialDevice, commandQueue))
                rt = threading.Thread(target=receiveThread, args=(serialDevice, responseQueue))
                st.daemon = True
                rt.daemon = True
                st.start()
                rt.start()
                threadscreated = True
            if setupmode and threadscreated and ModemDetect():
                startTime = time.time()
                currentState = "SetEcho"
                statetrans = False
                stateCommand(currentState, statetrans, commandQueue)
                while not currentState == "GetGPSstatus":
                    currentState, statetrans = getNEXTstate(currentState, responseQueue)
                    stateCommand(currentState, statetrans, commandQueue)
                while not ModemIPcheck(commandQueue, responseQueue):
                    time.sleep(1)
                setupmode = False
                print "[ OK ] System Setup Complete"
                print "*****************************"
            # Main program loop
            while (setupmode == False) and ModemDetect():
                currentState, statetrans = getNEXTstate(currentState, responseQueue)
                stateCommand(currentState, statetrans, commandQueue)
        except Queue.Empty:
            # Catch exceptions raised by put, or (more likely) get.
            #       - the sending thread ought to just keep emptying the commandQueue if the exception was Full,
            #       - we just have to wait for the reader thread to put something in the responseQueue if
            #         the exception was empty.
            pass
        except urllib2.HTTPError as e:
            # Capture HTTPError errors
            print " "
            print "**********"
            print "Exception detected (HTTPError)"
            print "Code:", e.code
            print "Reason:", e.reason
            print "**********"
        except urllib2.URLError as e:
            # Capture URLError errors
            print " "
            print "**********"
            print "Exception detected (URLError)"
            print "Reason:", e.reason
            print "**********"
            print "[ -- ] Rebooting Modem"
            setupmode = True
        except OSError as e:
            # Capture OS command line errors
            print " "
            print "**********"
            print "Exception detected (OSError)"
            print " **********"
            print e.errno
            print e.filename
            print e.strerror
            traceback.print_exc(file=sys.stdout)
            print "**********"
            sys.exit()
        except Exception as e:
            print " "
            print "**********"
            print "Exception detected"
            print type(e)
            print e
            traceback.print_exc(file=sys.stdout)
            print "**********"
            sys.exit()
        except KeyboardInterrupt:
            print " "
            print "**********"
            print "Exception (Keyboard Interrupt)"
            print "**********"
            sys.exit()
        except:
            print " "
            print "**********"
            print "Exception (Unknown)"
            traceback.print_exc(file=sys.stdout)
            print "********** "
            sys.exit()

if __name__ == "__main__":
    main()


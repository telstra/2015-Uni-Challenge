#!/usr/bin/python
"""
gpsclient - this program takes GPS location fixes using a Netgear GPS 
enabled AirCard 760S modem and posts the GPS lat and long to a Telstra hosted VM server.

Before running the script please change the team name listed in the global variables.

Once your team has set up their own server the script Json posts can be redirected 
to this sever by changing the positionURL within the Global variables.

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
this can result in communication conflicts.

"""


##########
# IMPORT #
##########

import json
import Queue
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

teamname = "Team XX"  # Change displayString value to reflect your team name
M2MserverUrl = "http://XXX.XXX.XXX.XXX/api/position"  # Target M2M server for JSON upload

DELAY = 10 # Seconds. Delay after JSON post.
startTime = 0

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

def SendOS(OScom):
    """Sends commands to Raspberry OS and returns output. """
    #print "[ -- ] Sending OS command:", OScom
    commandoutput = subprocess.Popen([OScom], shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    commandresult, commanderror = commandoutput.communicate()
    return commandresult, commanderror


def ModemDetect():
    """Checks if wireless modem is plugged into USB port and detectedby RaspberryPi"""
    #print "[ -- ] Checking Modem Present"
    commandresult, commanderror = SendOS('ls /dev/ttyUSB*')
    modemTTYinterface = re.search('ttyUSB3', commandresult) 
    # If modem plugged in and detected there should be a ttyUSB3 interface to communicate AT commands
    if modemTTYinterface and not commanderror:
        #print "[ OK ] Modem ttyUSB3 AT interface detected"
        result = True
    else:
        print "[FAIL] Modem ttyUSB3 interface is NOT detected, check sierra modem is plugged into USB port."
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


######################################################
# Code to handle uploading collected data to servers #
######################################################

def postJsonM2MServer(targetUrl, latitude, longitude, timedateUTC, imei, imsi, cpuID, displayString):
    """Upload values to targetUrl"""
    print "[ -- ] Posting data to M2M Server"
    cookieJar = CookieJar()
    arbitraryText = "UTC Timestamp is " + str(timedateUTC)
    print "       DisplayString:", str(displayString), " ArbitraryText:", str(arbitraryText)
    print "       Posting GPS coordinates lat:", str(latitude), "and long:", str(longitude)
    print "       imei:", str(imei), " imsi:", str(imsi), " cpuID:", str(cpuID)
    postdata = json.dumps({"latitude": str(latitude), "longitude": str(longitude), "timestampUTC": str(timedateUTC),
                    "imei": str(imei), "imsi": str(imsi), "cpuID": str(cpuID),
                    "displayString": str(displayString), "arbitraryText": str(arbitraryText)})
    o2 = urllib2.build_opener(urllib2.HTTPCookieProcessor(cookieJar))
    o2.addheaders = [ ('Content-Type', 'application/json')]
    resp2  = o2.open(targetUrl, postdata)
    UploadResult = resp2.getcode()
    if UploadResult == 200:
        print "[ OK ] Upload Sucessful"
    else:
        print "[FAIL] Upload HTTP Error", UploadResult
    return UploadResult


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
            #print "**************", r
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
                        print "[ -- ] Got REAL DATA and now posting LAT:", latitude, " Long:", longitude
                        result = postJsonM2MServer(M2MserverUrl, latitude, longitude, timedateUTC, imei, imsi, cpuID, teamname)
                        print result
                        time.sleep(DELAY)
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
                    #testpostresult = postJsonM2MServer(M2MserverUrl, -25.345, 131.035, '2013-01-01 00:00:00', imei, imsi, cpuID, teamname) # Test Command Only
                    #print testpostresult
                    #time.sleep(DELAY)
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
                if serialDevice.isOpen():
                    print "[ -- ] Serial port is already open"
                else:
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
                setupmode = False
                print "[ OK ] System Setup Complete"
                print "*****************************"
            # Main program loop
            while (setupmode == False) and ModemDetect():
                currentState, statetrans = getNEXTstate(currentState, responseQueue)
                stateCommand(currentState, statetrans, commandQueue)
                time.sleep(1)
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
            print "[ -- ] Check Raspberry Pi and modem have assigned IP addresses"
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


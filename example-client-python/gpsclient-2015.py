#! /usr/bin/python

import os
from gps import *
from time import *
import time
import threading

import subprocess
import urllib2
import re
import json

import traceback

import base64

####################
# Global Variables #
####################

teamname = "Team XX"  # Change value to reflect your team name

# Telstra VM Server Info
TelstraVMServerUrl = "http://XXX.XXX.XXX.XXX/api/position"  # Target M2M server for JSON upload
DELAY = 10 # Seconds. Delay between GPS fixes.
uploadfreq = 6 #Sets upload rate = (DELAY*uploadfreq) after JSON post.
uploadcounter = uploadfreq
gpsd = None #seting the global variable


##########################
# Code set up GPS thread #
##########################
# Written by Dan Mandle http://dan.mandle.me September 2012
# License: GPL 2.0

class GpsPoller(threading.Thread):
  def __init__(self):
    threading.Thread.__init__(self)
    global gpsd #bring it in scope
    gpsd = gps(mode=WATCH_ENABLE) #starting the stream of info
    self.current_value = None
    self.running = True #setting the thread running to true

  def run(self):
    global gpsd
    while gpsp.running:
      gpsd.next() #this will continue to loop and grab EACH set of gpsd info to clear the buffer


######################################################
# Code to handle uploading collected data to servers #
######################################################

def UploadJsonTelstraVM(targetUrl, latitude, longitude, timedateUTC, cpuID, displayString, cpuTEMP):
    """Uploads data via Json POST to Telstra VM Server"""
    print "[ -- ] Posting data to Telstra VM Server"
    arbitraryText = "UTC Timestamp is " + str(timedateUTC)
    #print "       DisplayString:", str(displayString), " ArbitraryText:", str(arbitraryText)
    #print "       Posting GPS coordinates lat:", str(latitude), "and long:", str(longitude)
    #print "       cpuID:", str(cpuID)
    postdata = json.dumps({
                    "cpuID":str(cpuID),
                    "displayString": str(displayString),
                    "latitude": str(latitude),
                    "longitude": str(longitude),
                    "arbitraryText": str(arbitraryText)
    })
    headers = {"Content-Type": "application/json"}
    print "     URL: ", targetUrl
    print "     Headers: ", headers
    print "     Body: ", postdata
    req = urllib2.Request(targetUrl, postdata, headers)
    req.get_method = lambda: 'POST'
    response = urllib2.urlopen(req)
    UploadResult = response.getcode()
    if UploadResult == 200:
        print "[ OK ] Upload Sucessful"
    else:
        print "[FAIL] Upload HTTP Error", UploadResult
    return UploadResult


########################
# Code to extract time #
########################

def getTimeUTC(r):
    """Extract time from GPS response and format for transmission to server"""
    #print "Extracting time from response"
    # Example Time: 2015-04-09T06:46:05.640Z
    z =  re.search('(\d+)-(\d+)-(\d+)T([\:\:\d]+)', r)

    if not z:
        return None
    else:
        # Now convert.
        yearUTC = z.group(1)
        monthUTC = z.group(2)
        dayUTC = z.group(3)
        timeUTC = z.group(4)
        timedateUTC = str(yearUTC) + "-" + str(monthUTC)  + "-" + str(dayUTC) + " " + str(timeUTC)
        # Example timedateUTC = 2015-04-09 06:46:05
        return timedateUTC

		
##########################################################
# Code to extract and format CPU ID information from Pi  #
##########################################################
	
def GetCPUid():
    """Queries Raspberry Pi for its unique CPU ID"""
    commandoutput = subprocess.Popen(["/bin/cat", "/proc/cpuinfo"], stdout=subprocess.PIPE)
    commandresult = commandoutput.communicate()[0]
    z =  re.search('Serial\s+\:\s+(.*)$', commandresult, re.MULTILINE)
    if z:
        #print ' CPU ID: ', z.group(1)
        return z.group(1)
    else:
        print '[FAIL] Unable to get CPU ID'
        return 0

def GetCPUtemp():
    """Queries Raspberry Pi for its unique CPU ID"""
    commandoutput = subprocess.Popen(["vcgencmd", "measure_temp"], stdout=subprocess.PIPE)
    commandresult = commandoutput.communicate()[0]
    z =  re.search('temp=([\.\d]+)', commandresult, re.MULTILINE)
    if z:
        #print ' CPU Temp: ', z.group(1)
        return z.group(1)
    else:
        print '[FAIL] Unable to get CPU ID'
        return 0


	
#############################################
# Main program loop with exception handlers #
#############################################

def main():
  global uploadcounter
  global uploadfreq
  global gpsp
  print '[ -- ] Starting Telstra University Challenge GPS Client 2015'
  print '[ -- ] Creating GPS Poller Thread'
  gpsp = GpsPoller() # create the thread
  try:
    gpsp.start() # start it up
    time.sleep(10) #It may take a second or two to get good data
    while True:
      #os.system('clear') #optional
      latitude = gpsd.fix.latitude
      longitude = gpsd.fix.longitude
      timedateUTC = getTimeUTC(gpsd.utc)
      cpuID = GetCPUid()
      cpuTEMP = GetCPUtemp()
      print '----------------------------------------'
      print '[    ]', teamname, ' Time UTC:', timedateUTC
      print '[    ] CPU ID:', cpuID, ' CPU Temp:', cpuTEMP
      print '[    ] Latitude:', str(latitude), ' Longitude:', str(longitude), ' Altitude (m):', gpsd.fix.altitude
      #print '[    ] eps', gpsd.fix.eps
      #print '[    ] epx', gpsd.fix.epx
      #print '[    ] epv', gpsd.fix.epv
      #print '[    ] ept', gpsd.fix.ept
      print '[    ] Speed (m/s):', gpsd.fix.speed, ' Climb:', gpsd.fix.climb
      print '[    ] Track', gpsd.fix.track
      print '[    ] Mode', gpsd.fix.mode
      #print ' '
      ##print '[    ] Sats', gpsd.satellites
      print '[    ] Satellites (total of', len(gpsd.satellites) , ' in view)'
      #for i in gpsd.satellites:
      #    print '\t', i

      if uploadcounter >= uploadfreq:
        if (str(latitude) == "nan") or (str(longitude) == "nan"):
            print '[ -- ] No GPS fix so nothing to upload'
        else:
          #print '[ -- ] Upload Data' 
          try:
            UploadJsonTelstraVM(TelstraVMServerUrl, str(latitude), str(longitude), timedateUTC, cpuID, teamname, cpuTEMP)
          except urllib2.HTTPError as e:
            # Capture HTTPError errors
            print " "
            print "******************************"
            print "Exception detected (HTTPError)"
            print "Code:", e.code
            print "Reason:", e.reason
            print "******************************"
            pass
          except urllib2.URLError as e:
            # Capture URLError errors
            print " "
            print "*****************************"
            print "Exception detected (URLError)"
            print "Reason:", e.reason
            print "*****************************"
            print "[ -- ] Check Raspberry Pi and modem have assigned IP addresses"
            pass
          uploadcounter = 1
      else:
          uploadcounter = uploadcounter + 1
      time.sleep(DELAY) #See Global variables for setting
	  

  except OSError as e:
    # Capture OS command line errors
    print " "
    print "****************************"
    print "Exception detected (OSError)"
    print "****************************"
    print e.errno
    print e.filename
    print e.strerror
    traceback.print_exc(file=sys.stdout)
    print "****************************"
    gpsp.running = False
    gpsp.join(30) # wait for the thread to finish what it's doing
    if gpsp.is_alive():
        gpsp.run = False
        print "Warning Timeout unable to join to gpsp thread"
        # Reset the gpsd
        os.system('sudo killall gpsd') 
        time.sleep(1)
        os.system('sudo gpsd /dev/ttyUSB0 -F /var/run/gpsd.sock')
        time.sleep(1)
    print "Done.\nExiting."
    sys.exit()
  except Exception as e:
    print " "
    print "******************"
    print "Exception detected"
    print "******************"
    print type(e)
    print e
    traceback.print_exc(file=sys.stdout)
    print "******************"
    gpsp.running = False
    gpsp.join(30) # wait for the thread to finish what it's doing
    if gpsp.is_alive():
        gpsp.run = False
        print "Warning Timeout unable to join to gpsp thread"
        # Reset the gpsd
        os.system('sudo killall gpsd')
        time.sleep(1)
        os.system('sudo gpsd /dev/ttyUSB0 -F /var/run/gpsd.sock')
        time.sleep(1)
    print "Done.\nExiting."
    sys.exit()
  except KeyboardInterrupt:
    print " "
    print "******************************"
    print "Exception (Keyboard Interrupt)"
    print "******************************"
    print "Killing Thread..."
    gpsp.running = False
    gpsp.join(30) # wait for the thread to finish what it's doing
    if gpsp.is_alive():
        gpsp.run = False
        print "Warning Timeout unable to join to gpsp thread"
        # Reset the gpsd
        os.system('sudo killall gpsd')
        time.sleep(1)
        os.system('sudo gpsd /dev/ttyUSB0 -F /var/run/gpsd.sock')
        time.sleep(1)
    print "Done.\nExiting."
    sys.exit()
  except:
    print " "
    print "*******************"
    print "Exception (Unknown)"
    print "*******************"
    traceback.print_exc(file=sys.stdout)
    print "*******************"
    print "Killing Thread..."
    gpsp.running = False
    gpsp.join(30) # wait for the thread to finish what it's doing
    if gpsp.is_alive():
        gpsp.run = False
        print "Warning Timeout unable to join to gpsp thread"
        # Reset the gpsd
        os.system('sudo killall gpsd')
        time.sleep(1)
        os.system('sudo gpsd /dev/ttyUSB0 -F /var/run/gpsd.sock')
        time.sleep(1)
    print "Done.\nExiting."
    sys.exit()

if __name__ == "__main__":
    main()


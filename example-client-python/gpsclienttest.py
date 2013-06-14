import gpsclient
import unittest
import Queue
import serial
import threading
import time
import os

class TestGpsClient(unittest.TestCase):
    def test_pdp_fail(self):
        commandQueue = Queue.Queue()
        responseQueue = Queue.Queue()
        serialDevice = serial.Serial(port='/dev/ttyUSB3', timeout=1)
        serialDevice.open()
        st = threading.Thread(target=gpsclient.sendThread, args=(serialDevice, commandQueue))
        rt = threading.Thread(target=gpsclient.receiveThread, args=(serialDevice, responseQueue))
        st.daemon = True
        rt.daemon = True
        st.start()
        rt.start()
        threadscreated = True
        # Test PDP fail connect
        print "Test PDP connect with wrong APN (telstra.fail)"
        command = 'ATE1'
        commandQueue.put(command + "\r\n", block=True, timeout=1)
        time.sleep(4)
        r = responseQueue.get(block=True, timeout=1)
        print r
        command = 'AT+CFUN=0'
        commandQueue.put(command + "\r\n", block=True, timeout=1)
        time.sleep(4)
        r = responseQueue.get(block=True, timeout=1)
        print r 
        command = 'AT+CFUN=1'
        commandQueue.put(command + "\r\n", block=True, timeout=1)
        time.sleep(10)
        r = responseQueue.get(block=True, timeout=1)
        print r
        command = 'AT+CGDCONT=1,\"IP\",\"telstra.fail\"'
        commandQueue.put(command + "\r\n", block=True, timeout=1)
        time.sleep(4)
        r = responseQueue.get(block=True, timeout=1)
        print r
        command = 'AT!SCDFTPROF=1'
        commandQueue.put(command + "\r\n", block=True, timeout=1)
        time.sleep(4)
        r = responseQueue.get(block=True, timeout=1)
        print r
        command = 'AT!SCPROF=1,"m2mChallenge",0,0,0,0'
        commandQueue.put(command + "\r\n", block=True, timeout=1)
        time.sleep(4)
        r = responseQueue.get(block=True, timeout=1)
        print r
        command = 'AT!SCACT=1,1'
        commandQueue.put(command + "\r\n", block=True, timeout=1)
        time.sleep(10)
        r = responseQueue.get(block=True, timeout=1)
        print r
        print "End Pdp fail test"
        print " "
        # Test PDP correct connect
        print "Test PDP connect with correct APN (telstra.extranet)"
        command = 'ATE1'
        commandQueue.put(command + "\r\n", block=True, timeout=1)
        time.sleep(4)
        r = responseQueue.get(block=True, timeout=1)
        print r
        command = 'AT+CFUN=0'
        commandQueue.put(command + "\r\n", block=True, timeout=1)
        time.sleep(4)
        r = responseQueue.get(block=True, timeout=1)
        print r
        command = 'AT+CFUN=1'
        commandQueue.put(command + "\r\n", block=True, timeout=1)
        time.sleep(10)
        r = responseQueue.get(block=True, timeout=1)
        print r
        command = 'AT+CGDCONT=1,\"IP\",\"telstra.extranet\"'
        commandQueue.put(command + "\r\n", block=True, timeout=1)
        time.sleep(4)
        r = responseQueue.get(block=True, timeout=1)
        print r
        command = 'AT!SCDFTPROF=1'
        commandQueue.put(command + "\r\n", block=True, timeout=1)
        time.sleep(4)
        r = responseQueue.get(block=True, timeout=1)
        print r
        command = 'AT!SCPROF=1,"m2mChallenge",0,0,0,0'
        commandQueue.put(command + "\r\n", block=True, timeout=1)
        time.sleep(4)
        r = responseQueue.get(block=True, timeout=1)
        print r
        command = 'AT!SCACT=1,1'
        commandQueue.put(command + "\r\n", block=True, timeout=1)
        time.sleep(10)
        r = responseQueue.get(block=True, timeout=1)
        print r
        os.system('sudo dhclient -nw wwan0')
        print "End Pdp correct test"
        print " "
        self.assertEqual('AT!SCACT=1,1\r\r\nOK\r\n', r)



    def test_getLongitude(self):
      x = gpsclient.getLongitude('Lon: 144 Deg 58 Min 16.33 Sec E')
      self.assertEqual(round(x,6), round(144.9712027777778, 6))


    def test_getLatitude(self):
      x = gpsclient.getLatitude('Lat: 37 Deg 48 Min 48.43 Sec S')
      self.assertEqual(round(x,6), round(-37.813452777777776, 6))


    def test_postJson(self):
      print "Team Name: Team Test"
      print "Target URL", gpsclient.positionUrl
      print "Posting test location Uluru -25.345000 and 131.035000 using time 2013-01-01 00:00:00"
      imei = '357000000000000'
      imsi = '505000000000000'
      cpuID = '0000000000000000'
      x = gpsclient.postJson(gpsclient.positionUrl, -25.345000, 131.035000, '2013-01-01 00:00:00', imei, imsi, cpuID, 'Team Test')
      print x
      self.assertEqual(200, x)
      

if __name__ == '__main__':
	unittest.main()

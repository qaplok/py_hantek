#!/usr/bin/python

import ctypes
from ctypes import *
import os
import sys
from struct import pack
from io import StringIO
import numpy as np
import time

# Set the directory for your HantekScope DLL here.
marchdll_file = os.path.join("Dll_x64", "HTHardDll.dll")
#marchdll_file_graph = os.path.join("Dll_x32", "HTDisplayDll.dll")

class Oscilloscope(object):
    def __init__(self, scopeid=0):  # Set up our scope. The scope id is for each scope attached to the system.
        # No Linux support...yet
        if os.name != 'nt':
            raise StandardError('Hantek SDK Oscilloscope wrapper only supports windows!')

        #self.marchdll = WinDLL(marchdll_file)
        self.marchdll = ctypes.CDLL(marchdll_file) #WinDLL(marchdll_file)
        #self.marchdll_graph = ctypes.CDLL(marchdll_file_graph)

        self.scopeid = c_ushort(scopeid)
        self.scop_id = np.array(self.scopeid, ctypes.c_ushort)
        
        self.LeverPos = [c_short(0), c_short(40), c_short(80), c_short(120)]
        self.CHVoltDIV = [c_short(9), c_short(9), c_short(9), c_short(9)]
        self.CHSet = c_ulong(14)
        self.TimeDIV = c_short(21)  #6-2V/div , 21- 10V/div
        self.YTFormat = c_short(0) #0-normal, 1-scan, 2-roll
        self.TriggerSource = c_short(0)
        
        class STRUCT_StControl(Structure):
            _fields_ = [("nCHSet", c_ulong),
                    ("nTimeDiv", c_short),
                    ("nTriggerSource", c_short),
                    ("nHTriggerPos", c_short),
                    ("nVTriggerPos", c_short),
                    ("nTriggerSlope", c_short),
                    ("nBufferLen", c_ulong),
                    ("nReadDataLen", c_ulong),
                    ("nAlreadyReadLen", c_ulong),
                    ("nALT", c_short)]

        self.stControl = STRUCT_StControl()

        self.stControl.nCHSet = self.CHSet
        self.stControl.nTimeDiv = self.TimeDIV
        self.stControl.nTriggerSource = self.TriggerSource#3
        self.stControl.nHTriggerPos = c_short(0) 
        self.stControl.nVTriggerPos = c_short(0)
        self.stControl.nTriggerSlope = c_short(0)
        self.stControl.nBufferLen = c_ulong(4096)
        self.stControl.nReadDataLen = c_ulong(4096)
        self.stControl.nAlreadyReadLen = c_ulong(0)
        self.stControl.nALT = c_short(0)
        
        self.nTriggerCouple = 1  # 0-DC, 1-AC, 2-LowFreq, 3-HighFreq
        self.DisLen = 2500; 
        self.StartNew = True
      
        class STRUCT_RelayControl(Structure):
            _fields_ = [("bCHEnable", c_ulong * 4),
                        ("nCHVoltDIV", c_short * 4),
                        ("nCHCoupling", c_ulong * 4),
                        ("bCHBWLimit", c_short * 4),
                        ("nTrigSource", c_short),
                        ("bTrigFilt", c_ulong),
                        ("nALT", c_short)]
                        
        self.rcRelayControl = STRUCT_RelayControl()
        
        for i in range(0,4):
            self.rcRelayControl.bCHEnable[i] = c_ulong(1)
            self.rcRelayControl.nCHVoltDIV[i] = c_short(9)
            self.rcRelayControl.nCHCoupling[i] = c_ulong(0) 
            self.rcRelayControl.bCHBWLimit[i] = c_short(0)
        
        self.rcRelayControl.nTrigSource = c_short(0)
        self.rcRelayControl.bTrigFilt = c_ulong(1)
        self.rcRelayControl.nALT = c_short(0)
        
        self.TriggerMode = c_short(0) #0-edge, 1-pulse, 2-video
        self.TriggerSlope = c_short(0) #0-increasing slop, 1-decreasing slope

        self.TriggerSweep = c_short(0)
        self.StartNew = True
            
        self.CH1SrcData = (c_short * 4096)()
        self.CH2SrcData = (c_short * 4096)()
        self.CH3SrcData = (c_short * 4096)()
        self.CH4SrcData = (c_short * 4096)()   
        
        

    def is_attached(self):
        """
            Takes no arguments.
            Returns true if the scope is attached, false otherwise.
        """
        
        #retval = self.marchdll.dsoOpenDevice(self.scopeid)
        retval = self.marchdll.dsoHTSearchDevice(self.scop_id.ctypes)
        if not retval:
            return False
        elif retval == 1:
            print("1. dsoHTSearchDevice")
            return True
        else:
            print("ERROR: Unexpected return value through API. dsoHTSearchDevice")
            return False


    def initHard(self):
        retval = self.marchdll.dsoInitHard(0)
        if not retval:
            return False
        elif retval == 1:
            print("2. InitHard")
            return True
        else:
            print("ERROR: Unexpected return value through API. InitHard")
            return False

    def HTADCCHModGain(self):
        retval = self.marchdll.dsoHTADCCHModGain(0, c_short(4)) #4
        if not retval:
            return False
        elif retval == 1:
            print("3. ModGain")
            return True
        else:
            print("ERROR: Unexpected return value through API. ModGain")
            return False

    def HTSetSampleRate(self):
        retval = self.marchdll.dsoHTSetSampleRate(0, byref(self.YTFormat), byref(self.rcRelayControl), byref(self.stControl))
        if not retval:
            return False
        elif retval == 1:
            print("4. HTSetSampleRate")
            return True
        else:
            print("ERROR: Unexpected return value through API. HTSetSampleRate")
            return False

    def HTSetCHAndTrigger(self):
        retval = self.marchdll.dsoHTSetCHAndTrigger(0, byref(self.rcRelayControl), byref(self.TimeDIV))
        if not retval:
            return False
        elif retval == 1:
            print("5. HTSetCHAndTrigger")
            return True
        else:
            print("ERROR: Unexpected return value through API. HTSetCHAndTrigger")
            return False

    def HTSetRamAndTrigerControl(self):
        retval = self.marchdll.dsoHTSetRamAndTrigerControl(0, byref(c_short(21)), byref(self.CHSet), byref(self.TriggerSource), byref(c_short(0)))
        if not retval:
            return False
        elif retval == 1:
            print("6. HTSetRamAndTrigerControl")
            return True
        else:
            print("ERROR: Unexpected return value through API. HTSetRamAndTrigerControl")
            return False
    
    def HTSetCHPos(self):
        for i in range(0,4):
            #retval = self.marchdll.dsoHTSetCHPos(0, byref(self.CHVoltDIV[i]), c_int16(self.LeverPos[i]), c_int16(i), c_int16(4))
            retval = self.marchdll.dsoHTSetCHPos(0, self.CHVoltDIV[i], self.LeverPos[i], c_short(i), c_short(4))

        if not retval:
            return False
        elif    retval == 1:
            print("7. HTSetCHPos")
            return True
        else:
            print("ERROR: Unexpected return value through API. HTSetCHPos")
            return False
        
    def HTSetVTriggerLevel(self):
        retval = self.marchdll.dsoHTSetVTriggerLevel(0, byref(c_short(0)), byref(c_short(4)))
        if not retval:
            return False
        elif retval == 1:
            print("8. HTSetVTriggerLevel")
            return True
        else:
            print("ERROR: Unexpected return value through API. HTSetVTriggerLevel")
            return False
            
    def HTSetTrigerMode(self):
        if (self.TriggerMode == 0):
            retval = self.marchdll.dsoHTSetTrigerMode(0, byref(self.TriggerMode), byref(self.TriggerSlope), byref(c_short(1)))
            if not retval:
                return False
            elif retval == 1:
                print("9. HTSetTrigerMode")
                return True
            else:
                print("ERROR: Unexpected return value through API. HTSetTrigerMode")
                return False

    

    #@staticmethod
    def CollectData(self):
        if (self.StartNew): 
            retval = self.marchdll.dsoHTStartCollectData(0, byref(c_short(1)))
            self.StartNew = False
            if not retval:
                print("FALSE. HTSetTrigerMode")
            #elif retval == 1:
                #print("10. HTSetTrigerMode OK")
                
  
        nState = self.marchdll.dsoHTGetState(0)
        if (nState and 2 == 2 ):
            self.read_data_from_scope(data_points=4096)
            self.StartNew = True
            #print("nState NEW")
        else:
            self.StartNew = False
            print("nState OLD")


    def read_data_from_scope(self, data_points=4096, display_points=4096, raw_data=False):
        """
            Takes two optional arguments, number of data points and number of display point
            to grab. Returns a tuple with channel 1 data, channel 2 data, time since capture init, and a trigger
            index on success, and None on failure.
        """
        
        '''
        data_ch1 = c_double(0)
        data_ch2 = c_double(0)
        data_ch3 = c_double(0)
        data_ch4 = c_double(0)
        '''
        data_ch1 = (c_short * data_points)()
        data_ch2 = (c_short * data_points)()
        data_ch3 = (c_short * data_points)()
        data_ch4 = (c_short * data_points)()
        
        t_index = c_ulong(0)

        
        #stControl = {'nCHSet': 7, 'nTimeDiv': 21, 'nTriggerSource': 0, 'nHTriggerPos': 0, 'nVTriggerPos': 0, 'nTriggerSlope': 0, 'nBufferLen': 4096,
        #'nReadDataLen': 4096, 'nAlreadyReadLen': 0, 'nALT': 0}
        #self.scopeid
    
        retval = self.marchdll.dsoHTGetData(0, byref(data_ch1), byref(data_ch2), byref(data_ch3), byref(data_ch4), byref(self.stControl))
        
        #retval = self.marchdll.dsoHTGetData(0, byref(data_ch1), byref(data_ch2), byref(data_ch3), byref(data_ch4), self.stControl.ctypes)
        
        #!!!retval = self.marchdll.dsoHTGetData(0, data_ch1_d.ctypes, data_ch2_d.ctypes, data_ch3_d.ctypes, data_ch4_d.ctypes, self.stControl.ctypes)
        
        if retval == -1:
            return None
        else:
            for i in range (0, 4096):
                self.CH1SrcData[i] = data_ch1[i] - (255 - 0)
                self.CH2SrcData[i] = data_ch2[i] - (255 - 40)
                self.CH3SrcData[i] = data_ch3[i] - (255 - 80)
                self.CH4SrcData[i] = data_ch4[i] - (255 - 120) # self.LeverPos[3])

    def GetDataFromDSO(self):
        t_index = c_ulong(0)
        koef =5 #0.96
        PPromMin = self.CH3SrcData[0]
        PPromMax = PPromMin
        for i in range (0, 4096):
            if self.CH3SrcData[i] < PPromMin:
                PPromMin = self.CH3SrcData[i]
            if self.CH3SrcData[i] > PPromMax:
                PPromMax = self.CH3SrcData[i]
        P3Min = PPromMin
        P3Max = PPromMax
        if P3Min < 0:
            meas_vpp3 = (P3Max + abs(P3Min)) 
            
        else:
            meas_vpp3 =(P3Max - P3Min) 
        meas_vpp3 = 0.00496245*meas_vpp3*meas_vpp3 + 0.28172529*meas_vpp3

        PPromMin = self.CH2SrcData[0]
        PPromMax = PPromMin
        for i in range (0, 4096):
            if self.CH2SrcData[i] < PPromMin:
                PPromMin = self.CH2SrcData[i]
            if self.CH2SrcData[i] > PPromMax:
                PPromMax = self.CH2SrcData[i]
        P2Min = PPromMin
        P2Max = PPromMax
        if P2Min < 0:
            meas_vpp2 = (P2Max + abs(P2Min))
        else:
            meas_vpp2 =(P2Max - P2Min)
        meas_vpp2 = 0.00496245*meas_vpp2*meas_vpp2 + 0.28172529*meas_vpp2
        PPromMin = self.CH4SrcData[0]
        PPromMax = PPromMin
        for i in range (0, 4096):
            if self.CH4SrcData[i] < PPromMin:
                PPromMin = self.CH4SrcData[i]
            if self.CH4SrcData[i] > PPromMax:
                PPromMax = self.CH4SrcData[i]
        P4Min = PPromMin
        P4Max = PPromMax
        if P4Min < 0:
            meas_vpp4 = (P4Max + abs(P4Min))
        else:
            meas_vpp4 =(P4Max - P4Min) 
        meas_vpp4 = 0.00496245*meas_vpp4*meas_vpp4 + 0.28172529*meas_vpp4
        return self.CH1SrcData, self.CH2SrcData, self.CH3SrcData, self.CH4SrcData, [j / 1e6 for j in range(0, 4097)], t_index, meas_vpp2, meas_vpp3, meas_vpp4

  

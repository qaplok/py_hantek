import time

import matplotlib.backends
import matplotlib

matplotlib.use('Qt5Agg') 
import numpy as np
import matplotlib.pylab as plt
import matplotlib.rcsetup as rcsetup
print(rcsetup.all_backends)
from matplotlib.figure import Figure
import matplotlib.animation as animation
from HTSDKScope_my import Oscilloscope
from collections import deque

if __name__ == "__main__":
    #['GTK3Agg', 'GTK3Cairo', 'GTK4Agg', 'GTK4Cairo', 'MacOSX', 'nbAgg', 'QtAgg', 'QtCairo', 'Qt5Agg', 'Qt5Cairo', 'TkAgg', 
    # 'TkCairo', 'WebAgg', 'WX', 'WXAgg', 'WXCairo', 'agg', 'cairo', 'pdf', 'pgf', 'ps', 'svg', 'template']
    scope0 = Oscilloscope(scopeid=0)
    if not scope0.is_attached():
        print("WARNING: Scope not found!")
        exit()
    
    scope0.initHard()
    scope0.HTADCCHModGain()
    scope0.HTSetSampleRate()
    scope0.HTSetCHAndTrigger()
    scope0.HTSetRamAndTrigerControl()
    scope0.HTSetCHPos()
    scope0.HTSetVTriggerLevel()
    scope0.HTSetTrigerMode()
    

    #scope0.set_voltage_division(1, 5)
    #print(scope0.set_sampling_rate(26))
    #scope0.setup_dso_cal_level()
    #plt.ion()
 
    
    fig, ax = plt.subplots(figsize=(6, 4))
    fig.set_facecolor('gray')
    ax.set_facecolor('white')
    ax.set_xlim([-100, 5000])
    ax.set_ylim([-270, 270])
    line, = ax.plot([], [], lw=3)

   

    def animate(i):
        length = 4096
        scope0.CollectData()
        data = scope0.GetDataFromDSO()
        tIndex = data[5].value
       
        #x.append(x[-1] + 1)  # update data
        #y.append(y[-1] + dy)
        y = data[1][:length]
        x = data[4][tIndex:tIndex + length]
        #line.set_data(x, y)
        ax.clear()
        ax.plot(x,y)
        '''
        ax.relim()  # update axes limits
        ax.autoscale_view(True, True, True)
        return line, ax
        '''
        #data = scope0.read_data_from_scope(data_points=4096)#3000)
        #pylab.plot(data[0][:length], data[0][tIndex:tIndex + length])  # , 'r-')
        #ax.plot(data[1][:length], data[0][tIndex:tIndex + length])
        #pylab.plot(data[2][:length], data[0][tIndex:tIndex + length])
        #pylab.plot(data[3][:length], data[0][tIndex:tIndex + length])
        #plt.draw()
        #plt.show()
        #time.sleep(1)
    
    
    


    ani = animation.FuncAnimation(fig, animate, interval=100)
    plt.show()
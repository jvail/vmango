from temperature import get_temperature

class AbstractThermalTimeAccumulator:
    def __init__(self, initttsum = 0):
        self.ttsum = initttsum # the thermal time sum

    def accumulate(self, temperature, duration = 1):
        raise NotImplementedError()

    def release(self, temperature, duration = 1):
        raise NotImplementedError()

    def get_ttsum(self):
        return self.ttsum

    def set_ttsum(self, value):        
        self.ttsum = value


    def accumulate_daytemperature(self, day, get_temperature = get_temperature):
        self.accumulate(get_temperature(day))

    def accumulate_dayrange_temperature(self, firstday, lastday, get_temperature = get_temperature):
        for day in date_xrange(firstday, lastday+timedelta(days=1)):
            self.accumulate(get_temperature(day),1)
  
    def find_date_of_accumulation(self, targetttsum, initialdate, get_temperature = get_temperature, initialtsum = None):
        from datetime import timedelta

        previoustsum = self.get_ttsum()
        if not initialtsum is None:
            self.set_ttsum(initialtsum)
        
        temp = get_temperature(initialdate)
        self.accumulate(temp)
        cdate = initialdate

        while self.ttsum < targetttsum:
            cdate += timedelta(days=1)
            temp = get_temperature(cdate)
            self.accumulate(temp)

        self.set_ttsum(previoustsum)

        return cdate


    def reverse_from_finaldate(self, targetttsum, finaldate, get_temperature = get_temperature):
        from datetime import timedelta
        temp = get_temperature(finaldate)
        self.release(temp)
        cdate = finaldate
        while self.ttsum > targetttsum:
            cdate -= timedelta(days=1)
            temp = get_temperature(cdate)
            self.release(temp)
        self.set_ttsum(targetttsum)
        return cdate



class ThermalTimeAccumulator (AbstractThermalTimeAccumulator):
    def __init__(self, basetemperature, initsum = 0):
        AbstractThermalTimeAccumulator.__init__(self, initsum)
        self.basetemperature = basetemperature

    def accumulate(self, temperature, duration = 1):
        self.ttsum += max(0,temperature - self.basetemperature) * duration

    def release(self, temperature, duration = 1):
        self.ttsum -= max(0,temperature - self.basetemperature) * duration



class MultiPhaseThermalTimeAccumulator (AbstractThermalTimeAccumulator):
    def __init__(self, basetemperatures, stage_tempsum, initsum = 0, stagesnames = None):
        AbstractThermalTimeAccumulator.__init__(self, initsum)
        import numpy as np
        self.basetemperatures = basetemperatures
        self.stage_tempsum = stage_tempsum
        self.stagechangetempsum = np.cumsum(stage_tempsum)
        self.nbstagechange = len(self.stagechangetempsum)
        self.accumafter = (len(self.basetemperatures) == self.nbstagechange+1)
        self.stagesnames = stagesnames

        self.set_ttsum(initsum)

    def set_ttsum(self, ttsum = 0 ):
        self.ttsum = ttsum # the thermal time sum
        self.stage = self.find_stage(ttsum)


    def accumulate(self, temperature, duration = 1):
        if self.stage ==  self.nbstagechange: 
            if self.accumafter : 
                self.ttsum += max(0,temperature - self.basetemperatures[self.stage]) * duration
        else:
            delta = max(0,temperature - self.basetemperatures[self.stage])
            cumulatedtemp =  delta * duration
            ttsum = self.ttsum + cumulatedtemp
            if self.stage < self.nbstagechange and ttsum > self.stagechangetempsum[self.stage]:
                duration = (self.stagechangetempsum[self.stage] - self.ttsum) / float(ttsum - self.ttsum)
                self.ttsum = self.stagechangetempsum[self.stage]
                self.stage += 1
                self.accumulate(temperature, duration)
            else:
                self.ttsum = ttsum

    def release(self, temperature, duration = 1):
        if self.stage ==  self.nbstagechange and not self.accumafter : 
            raise ValueError('Cannot release on last stage')
        else:
            delta = max(0,temperature - self.basetemperatures[self.stage])
            cumulatedtemp =  delta * duration
            ttsum = self.ttsum - cumulatedtemp
            if self.stage > 0 and ttsum < self.stagechangetempsum[self.stage-1]:
                duration = (self.stagechangetempsum[self.stage-1] - self.ttsum) / float(ttsum - self.ttsum)
                self.ttsum = self.stagechangetempsum[self.stage-1]
                self.stage -= 1
                self.release(temperature, duration)
            else:
                self.ttsum = ttsum

    def rank_in_stage(self):
        if self.stage >= self.nbstagechange: return 0
        elif self.stage == 0:
            return self.ttsum / self.stagechangetempsum[0]
        else: 
            return (self.ttsum - self.stagechangetempsum[self.stage-1]) / (self.stagechangetempsum[self.stage]  - self.stagechangetempsum[self.stage-1])
    
    def find_stage(self, ttsum):
        if ttsum < self.stagechangetempsum[0]:
            return 0
        elif ttsum >= self.stagechangetempsum[-1]:
            return self.nbstagechange
        else:
            for st, stagechangettsum in reversed(list(enumerate(self.stagechangetempsum))):
                if ttsum >= stagechangettsum: 
                    return st+1
            return 0

    def find_date_of_stage_begin(self, stagename, initialdate, get_temperature = get_temperature, initialtsum = None):
        if self.stagesnames:
            stagepos = self.stagesnames.index(stagename)
        else : stagepos = stagename
        targetttsum = self.stagechangetempsum[stagepos-1] if stagepos > 0 else 0
        return self.find_date_of_accumulation(targetttsum, initialdate, get_temperature)

    def find_date_of_stage_end(self, stagename, initialdate, get_temperature = get_temperature, initialtsum = None):
        if self.stagesnames:
            stagepos = self.stagesnames.index(stagename)
        else : stagepos = stagename
        targetttsum = self.stagechangetempsum[stagepos]
        return self.find_date_of_accumulation(targetttsum, initialdate, get_temperature)




def test():
    from vplants.mangosim.temperature import init_temperatures, get_temperature
    import numpy as np

    init_temperatures()
    
    pheno_base_temp_Inflo   = [11.10,5.38,8.67,15.11,0]                # base temperature for each phenological stage of inflorescence
    pheno_stade_temp_Inflo  = [70.56,172.35,133.32,230.42]
    pheno_change_temp_Inflo = np.cumsum (pheno_stade_temp_Inflo)

    pheno_tts    = MultiPhaseThermalTimeAccumulator(pheno_base_temp_Inflo, pheno_change_temp_Inflo, 350)
    from datetime import date
    return pheno_tts.reverse_from_finaldate(0, date(2003,8,15), get_temperature)


if __name__ == '__main__':
    test()

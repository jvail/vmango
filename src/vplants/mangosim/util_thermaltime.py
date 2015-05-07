
class ThermalTimeAccumulator:
    def __init__(self, basetemperature, initsum = 0):
        self.ttsum = initsum # the thermal time sum
        self.basetemperature = basetemperature

    def accumulate(self, temperature, duration = 1):
        self.ttsum += max(0,temperature - self.basetemperature) * duration


class MultiPhaseThermalTimeAccumulator:
    def __init__(self, basetemperatures, stagechangetempsum, initsum = 0):
        self.stage = 0
        self.basetemperatures = basetemperatures
        self.stagechangetempsum = stagechangetempsum
        self.nbstagechange = len(self.stagechangetempsum)
        self.ttsum = 0 # the thermal time sum
        if initsum > 0: 
            self.accumulate(initsum, 1)

    def accumulate(self, temperature, duration = 1):
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

    def rank_in_stage(self):
        if self.stage >= self.nbstagechange: return 0
        elif self.stage == 0:
            return self.ttsum / self.stagechangetempsum[0]
        else: 
            return (self.ttsum - self.stagechangetempsum[self.stage-1]) / (self.stagechangetempsum[self.stage]  - self.stagechangetempsum[self.stage-1])


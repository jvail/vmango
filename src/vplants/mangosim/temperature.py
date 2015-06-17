from datetime import datetime, date

__temperatures = None
__first_day = None

def init_temperatures(filename = 'meteo-BMA.csv'):
    from vplants.mangosim.util_path import share_dir, join
    from pandas import read_csv

    global  __temperatures, __first_day
    date_parser = lambda d : datetime.strptime(d, '%d/%m/%y') # date are written in a french style
    temperature_data = read_csv(join(share_dir, filename), parse_dates = [0],date_parser=date_parser,delimiter=';')
    
    __temperatures = temperature_data['Tmoy']
    __first_day = temperature_data['Date'][0]
    
    #for i in xrange(1,len(temperature_data['Date'])):
    #    assert (temperature_data['Date'][i] - temperature_data['Date'][i-1]).days == 1

def get_first_day_for_temperature():
    return __first_day

def get_last_day_for_temperature():
    from datetime import timedelta
    return __first_day+timedelta(days=len(__temperatures)-1)

def get_temperature(date):
    if type(date) == datetime:  return __temperatures[(date-__first_day).days]
    else : return __temperatures[(date-__first_day.date()).days]

if __name__ == '__main__':
    init_temperatures()

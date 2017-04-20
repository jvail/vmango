from datetime import date, timedelta


Month = {'janv' : 1, 'fev' : 2, 'mars' : 3,
         'avril' : 4, 'mai' : 5, 'juin' : 6,
         'juil' : 7, 'aout' : 8, 'sept' : 9,
         'oct' : 10, 'nov' : 11, 'dec' : 12 }

MonthEn = {'jan' : 1, 'feb' : 2, 'march' : 3,
         'april' : 4, 'may' : 5, 'june' : 6,
         'july' : 7, 'august' : 8, 'sept' : 9,
         'oct' : 10, 'nov' : 11, 'dec' : 12 }


MonthName = dict([(v,k) for k,v in Month.items()])
MonthEnName = dict([(v,k) for k,v in MonthEn.items()])


MonthOrder = range(6,13)+range(6)

def date_from_string(string):
    """From string = 'month.year', it return a date
    """
    m,y = string.split(".")
    date_ = date(2000+int(y), Month[m], 1)
    return date_


def month_difference(d1,d2):
    return (d1.year-d2.year)*12 + (d1.month-d2.month)

def week_difference(d1,d2):
    from math import ceil
    return ceil((d2 -d1).days/7.)

# Fred note : The actual cycle seems to start at beginning of June
def vegetative_cycle_begin(cycle):
    return date(2000+cycle-1,6,1)
    # return date(2000+cycle-1,7,1)

def vegetative_cycle_end(cycle):
   return date(2000+cycle,5,31)
   # return date(2000+cycle,6,30)

def in_vegetative_cycle(cdate, cycle):
    if type(cdate) != date: cdate = cdate.date()
    return vegetative_cycle_begin(cycle) <= cdate <= vegetative_cycle_end(cycle)


def get_vegetative_cycle(cdate):
    if type(cdate) != date: cdate = cdate.date()
    if cdate < vegetative_cycle_begin(4) : return 3
    elif cdate < vegetative_cycle_begin(5) : return 4
    else : return 5

def get_semester(cdate):
    if cdate.month >= vegetative_cycle_begin(4).month : return 1
    else : return 2

def flowering_cycle_begin(cycle):
    return date(2000+cycle, 7, 1)

def flowering_cycle_end(cycle):
    return date(2000+cycle, 10, 31)

def in_flowering_cycle(cdate, cycle):
    if type(cdate) != date: cdate = cdate.date()
    return flowering_cycle_begin(cycle) <= cdate <= flowering_cycle_end(cycle)

def get_flowering_cycle(date):
    return date.year - 2000

def fruiting_cycle_begin(cycle):
    return date(2000+cycle, 11, 1)

def fruiting_cycle_end(cycle):
    return date(2000+cycle+1, 3, 31)

def in_fruiting_cycle(cdate, cycle):
    if type(cdate) != date: cdate = cdate.date()
    return fruiting_cycle_begin(cycle) <= cdate <= fruiting_cycle_end(cycle)

def get_fruiting_cycle(date):
    c = get_flowering_cycle(date)
    if in_fruiting_cycle(date,c) : return c
    else : return c - 1

def date_range(begindate, enddate, daystep = 1):
    if enddate <= begindate: return []
    currentdate = begindate
    if type(daystep) == timedelta: delta = daystep
    else: delta = timedelta(days=daystep)
    res = []
    while currentdate < enddate :
        res.append(currentdate)
        currentdate += delta
    return res

def date_xrange(begindate, enddate, daystep = 1):
    currentdate = begindate
    if type(daystep) == timedelta: delta = daystep
    else: delta = timedelta(days=daystep)
    while currentdate  < enddate :
        yield currentdate
        currentdate += delta


def monthdate_range(begindate, enddate):
    if enddate <= begindate: return []
    currentmonth, currentyear = begindate.month, begindate.year
    endmonth, endyear = enddate.month, enddate.year
    res = []
    while currentyear < endyear or (currentmonth < endmonth and currentyear == endyear):
        res.append((currentyear, currentmonth))
        currentmonth += 1
        if currentmonth > 12:
            currentyear += 1
            currentmonth -= 12
    return res


def monthdate_xrange(begindate, enddate):
    currentmonth, currentyear = begindate.month, begindate.year
    endmonth, endyear = enddate.month, enddate.year
    while currentyear < endyear or (currentmonth < endmonth and currentyear == endyear):
        yield (currentyear, currentmonth)
        currentmonth += 1
        if currentmonth > 12:
            currentyear += 1
            currentmonth -= 12

def weekdate_range(begindate, enddate):
    currentweek, currentyear = get_week_from_date(begindate), begindate.year
    endweek, endyear = get_week_from_date(enddate), enddate.year
    maxweek = get_week_from_date(date(currentyear, 12, 28))
    res = []
    while currentyear < endyear or (currentweek < endweek and currentyear == endyear):
        res.append((currentyear,currentweek))
        currentweek += 1
        if currentweek > maxweek:
            currentyear += 1
            currentweek -= maxweek
            maxweek = get_week_from_date(date(currentyear, 12, 28))
    return res

def weekdate_xrange(begindate, enddate):
    currentweek, currentyear = get_week_from_date(begindate), begindate.year
    endweek, endyear = get_week_from_date(enddate), enddate.year
    maxweek = get_week_from_date(date(currentyear, 12, 28))
    while currentyear < endyear or (currentweek < endweek and currentyear == endyear):
        yield (currentyear,currentweek)
        currentweek += 1
        if currentweek > maxweek:
            currentyear += 1
            currentweek -= maxweek
            maxweek = get_week_from_date(date(currentyear, 12, 28))


#beg_end_period = {'E' : (7,8,9,10), 'I' : (11,12,1,2), 'L' : (3,4,5,6)}

def get_date_from_week(year, week, weekday):
    from datetime import datetime
    assert 1 <= weekday <= 7
    return datetime.strptime(str(year)+'/'+str(week)+'/'+str(weekday%7),'%Y/%W/%w').date()

def get_week_from_date(date):
    return date.isocalendar()[1]
    
def get_weekday_from_date(date):
    return date.isocalendar()[2]
    

#
bloom_weeks_03 = { 0 : (date(2003,7,1),get_date_from_week(2003,31,7)) }
bloom_weeks_03.update(dict([(i, (get_date_from_week(2003,31+i,1), get_date_from_week(2003,31+i,7))) for i in xrange(1,13)]))  

bloom_weeks_04 = { 0 : (date(2004,7,1),get_date_from_week(2004,31,7)) }
bloom_weeks_04.update(dict([(i, (get_date_from_week(2004,31+i,1), get_date_from_week(2004,31+i,7))) for i in xrange(1,13)]))  

bloom_weeks_05 = { 0 : (date(2005,7,1),get_date_from_week(2005,31,7)) }
bloom_weeks_05.update(dict([(i, (get_date_from_week(2005,31+i,1), get_date_from_week(2005,31+i,7))) for i in xrange(1,13)]))   

bloom_weeks = {3 : bloom_weeks_03, 4 : bloom_weeks_04, 5 : bloom_weeks_05}


def get_bloom_week(date, icycle = None):
    if icycle is None:
        icycle = get_flowering_cycle(date)
    bweeks = bloom_weeks[icycle]
    for periodid, period_beg_end  in bweeks.items():
        if period_beg_end[0] <= date <= period_beg_end[1]:
            return periodid

harvest_weeks_03 = dict([(i, (get_date_from_week(cyear,cweek,1), get_date_from_week(cyear,cweek,7))) for i, (cyear, cweek) in enumerate(weekdate_xrange(date(2003, 12, 15), date(2004, 3, 15)))])
harvest_weeks_04 = dict([(i, (get_date_from_week(cyear,cweek,1), get_date_from_week(cyear,cweek,7))) for i, (cyear, cweek) in enumerate(weekdate_xrange(date(2004, 12, 15), date(2005, 3, 15)))])
harvest_weeks_05 = dict([(i, (get_date_from_week(cyear,cweek,1), get_date_from_week(cyear,cweek,7))) for i, (cyear, cweek) in enumerate(weekdate_xrange(date(2005, 12, 15), date(2006, 3, 15)))])

harvest_weeks = {3 : harvest_weeks_03, 4 : harvest_weeks_04, 5 : harvest_weeks_05}

def get_harvest_week(date, icycle = None):
    if icycle is None:
        icycle = get_fruiting_cycle(date)
    weeks = harvest_weeks[icycle]
    for periodid, period_beg_end  in weeks.items():
        if period_beg_end[0] <= date <= period_beg_end[1]:
            return periodid

def todatetime(d): return d

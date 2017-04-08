
def send_msg(subject, msg):
    import yagmail
    yag =yagmail.SMTP('fboudon@cirad.fr',host='smtp.cirad.fr')
    yag.send(to='frederic.boudon@cirad.fr',subject=subject,contents=msg)
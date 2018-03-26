
LARGE_REPORT_HEADER =  """\documentclass[10pt,a4paper]{article}
\usepackage[utf8]{inputenc}
\usepackage[english]{babel}

\usepackage{amsmath}
\usepackage{amsfonts}
\usepackage{amssymb}
\usepackage{booktabs}
\usepackage{longtable}
\usepackage{hyperref}
\usepackage{graphicx}
\usepackage[left=1cm,right=1cm,top=1cm,bottom=1.5cm]{geometry}
"""

BEGIN_DOCUMENT = "\\begin{document}\n"
MAKE_TITLE = "\maketitle\n"
CLEARPAGE = "\\clearpage\n"


TITLE_PAGE = """
\\begin{titlepage}
\\begin{center}
\\vspace*{\\fill}
{\Huge %s }\\\\[0.5cm]
{\Large %s }\\\\[0.4cm]
\\today\\\\[0.4cm]
\\vspace*{\\fill}
{\Large %s}
\end{center}
\end{titlepage}

"""
import os
pdffilename = lambda texfilename: os.path.splitext(texfilename)[0]+'.pdf'

class TexReportGenerator:
    def __init__(self, fname, title, subtitle = ''):
        self.fname = fname
        self.compiled = False
        self.texstream = file(fname, 'w')
        # Add 'large_report_header_jo.tex' input as fisrt line:
        self.texstream.write(LARGE_REPORT_HEADER)
        # Begin document:
        self.texstream.write(BEGIN_DOCUMENT)
        # Add title & subtitle infos:
        self._make_titlepage(title, subtitle)
        self.add_table_of_contents()

    def __del__(self):
        if self.texstream: self.close()

    def close(self):
        self.write("\end{document}\n")
        self.texstream = None

    def compile(self, fastcompilation = None):
        if self.texstream: self.close()
        import os
        cwd = os.getcwd()
        twd = os.path.dirname(self.fname)
        os.chdir(twd)
        texbasefname = os.path.basename(self.fname)
        genfilename = "generationfile.tex"
        toc = os.path.splitext(genfilename)[0]+'.toc'
        if fastcompilation is None:
            md5sum = None
            if os.path.exists(toc):
                import md5
                md5sum = md5.new(file(toc,'r').read()).digest()

        genfile = file(genfilename,'w')
        genfile.write("\input{"+os.path.splitext(texbasefname)[0]+"}\n\n")
        genfile.close()
        os.system('pdflatex -interaction=batchmode '+genfilename)
        if fastcompilation is None and not md5sum is None:
            nmd5sum = md5.new(file(toc,'r').read()).digest()
            fastcompilation = (md5sum == nmd5sum)
        if not fastcompilation: os.system('pdflatex -interaction=batchmode '+genfilename)
        os.rename(pdffilename(genfilename),pdffilename(texbasefname))
        self.compiled = True
        os.chdir(cwd)

    def view(self):
        if not self.compiled : self.compile()
        import os
        pdffile = os.path.splitext(self.fname)[0]+'.pdf'
        os.system('open '+pdffile)

    def viewtex(self):
        import os
        os.system('open '+self.fname)

    def _make_titlepage(self, title, subtitle='' , author = None):
        """
        Create a latex 'titlepage' section.
        """
        import platform
        if author == None:
            import getpass
            author = getpass.getuser()
        authorship = "Build on " + platform.uname()[1] + " (" + " ".join(platform.dist()) + ") by "+ author
        self.write(TITLE_PAGE % (title, authorship, subtitle))
        self.clear_page()

    def add_table_of_contents(self):
        self.write("\\tableofcontents")
        self.clear_page()

    def clear_page(self):
        self.write(CLEARPAGE)

    def add_section(self, title):
        self.write('\\section{'+title+'}\n\n')

    def add_subsection(self, title):
        self.write('\\subsection{'+title+'}\n\n')

    def add_subsubsection(self, title):
        self.write('\\subsubsection{'+title+'}\n\n')

    def add_figure(self, fnames, caption = None) :
        self.write('\\begin{figure}[h]\n\t\centering\n')
        if type(fnames) == str:
            fnames = [fnames]
        nbfnames = len(fnames)
        ratio = 0.9/nbfnames
        for fname in fnames:
            self.write("\t\includegraphics[width=%f\linewidth]{%s}\n" % (ratio, fname))
        if caption: self.write('\t\\caption{%s}\n' % caption)
        self.write('\\end{figure}\n\n')

    def add_table(self, table, caption = None, precision = 3):
        #self.write('\\begin{table}[H]\n')
        self.write('\\begin{center}\n')
        self.write(table.to_latex(float_format=lambda f: ('%.'+str(precision)+'f') % f))
        self.write('\n\\end{center}\n')
        #if caption: self.write('\t\\caption{%s}\n' % caption)
        #self.write('\\end{table}\n')


    def write(self, text):
        assert not self.texstream is None
        self.texstream.write(text)

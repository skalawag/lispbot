MAKEINFO=makeinfo
MAINFILE=lispbot.texi
MANUAL_NAME=lispbot_manual

TEXT_OPTS=--no-headers --paragraph-indent=none
PDF_CMD=texi2pdf

all: text info pdf html

INSTALL: installation.texi
	$(MAKEINFO) $(TEXT_OPTS) -o $@ $<

README: basic_usage.texi
	$(MAKEINFO) $(TEXT_OPTS) -o $@ $<

$(MANUAL_NAME).txt: $(MAINFILE) *.texi
	$(MAKEINFO) $(TEXT_OPTS) -o $@ $<

$(MANUAL_NAME).info: $(MAINFILE) *.texi
	$(MAKEINFO) $<

$(MANUAL_NAME).pdf: $(MAINFILE) *.texi
	$(PDF_CMD) -o $@ $<

$(MANUAL_NAME).html: $(MAINFILE) *.texi
	$(MAKEINFO) --html --no-headers --no-split -o $@ $<

$(MANUAL_NAME): $(MAINFILE) *.texi
	$(MAKEINFO) --html -o $@ $<

text: INSTALL README $(MANUAL_NAME).txt

info: $(MANUAL_NAME).info

pdf: $(MANUAL_NAME).pdf

html: $(MANUAL_NAME).html $(MANUAL_NAME)


clean_garbage:
	rm *.aux *.cp *.fn *.fns *.ky *.log *.pg *.toc *.tp *.vr

clean: 
	rm lispbot_manual.{html,pdf,txt,info} INSTALL README
	rm -r lispbot_manual

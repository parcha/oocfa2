VER = v1-0-1
VERSION = 1.0.1

APP0 = copris
APP = $(APP0)-$(VER)
JAR = $(APP).jar
ZIP = $(APP).zip

WEBPAGE = http://bach.istc.kobe-u.ac.jp/copris/
WEBTITLE = Copris: Constraint Programming in Scala
JARS = lib/sugar-v1-15-0.jar:lib/org.sat4j.core.jar
SRCS = src/jp/kobe_u/*.scala src/jp/kobe_u/copris/*.scala src/jp/kobe_u/copris/sugar/*.scala

DOCTITLE = Copris version $(VERSION) Core API Specification
SCALADOC  = scaladoc \
	-d docs/api \
	-doc-title '$(DOCTITLE)' \
	-doc-version '$(VERSION)' \
	-classpath classes:$(JARS) \
	-sourcepath src
#	-doc-source-url 'http://bach.istc.kobe-u.ac.jp/copris/current/src/€{FILE_PATH}.scala'

all: scalac jar scaladoc zip

scalac:
	rm -rf classes/*
	fsc -sourcepath src -d classes -cp $(JARS) -optimise $(SRCS)
#	scalac -sourcepath src -d classes -cp $(JARS) -optimise $(SRCS)

jar:
	jar cf ../$(JAR) -C classes .
	cp -p ../$(JAR) lib/

scaladoc:
	rm -rf docs/api/*
	$(SCALADOC) $(SRCS)

zip:
	rm -f ../$(ZIP)
	rm -rf $(APP)
	mkdir $(APP)
	cp -pr Makefile src lib docs examples $(APP)
	rm -f $(APP)/lib/copris*.jar $(APP)/lib/org.sat4j.core.jar $(APP)/examples/classes/*
	cp -pr ../$(JAR) $(APP)/lib
	find $(APP) \( -name .svn -o -name CVS -o -name .cvsignore -o -name '*~' \) -exec rm -r '{}' '+'
	zip -q -r ../$(ZIP) $(APP)
	rm -rf $(APP)

clean:
	rm -rf classes/*
	rm -rf docs/api/*

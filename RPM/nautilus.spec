%define     SourceName   nautilus
Name:       nautilus-securephone
Version:    1.8rc3
Release:    2
Group:      Multimedia/Audio
Summary:    secure dialup/IP voice phone	
License:    proprietary / free for non commercial use
Source:     http://download.berlios.de/nautilus/%{SourceName}-%{version}.tar.gz
BuildRoot:  %{_tmppath}/%{SourceName}-%{version}-build
Requires: alsa-oss

%define SHAREPACK /usr/share/packages
%define SHAREDIR %{SHAREPACK}/%{Name}

%Description
Nautilus secure phone allows half-duplex encrypted-voice conversation over standard 
phone lines or network links.  It requires a PC or Sun Sparcstation with 
speakers and microphone.  Additionally, a 4800 bps or faster modem or 
IP network connection to another computer running nautilus is required.

------
Authors: William Dorsey, Andrew Fingerhut, et al.

%prep
%setup -n %{SourceName}-%{version}
echo 'LOGON_FILE = "%{SHAREDIR}/logon_new.v"' >>nautilus.cfg
echo 'RING_FILE = "%{SHAREDIR}/ring_new.v"'   >>nautilus.cfg

%build
# Force use of safer macros
#%{fixUP} -g conf_script groff 'groff -S'
make CC="gcc $RPM_OPT_FLAGS" linux
strip nautilus

%install
echo "Build Environment gives us RPM_BUILD_ROOT=$RPM_BUILD_ROOT"
if [ -z "$RPM_BUILD_ROOT" ] 
then
  if [ -n "%{buildroot}" ]
  then
    RPM_BUILD_ROOT="%{buildroot}"
  else 
    RPM_BUILD_ROOT=/tmp/nautilus/$$
    echo "define own RPM_BUILD_ROOT: $RPM_BUILD_ROOT"
  fi
  rm -rf $RPM_BUILD_ROOT
  mkdir -p $RPM_BUILD_ROOT
fi

DESTDIR=$RPM_BUILD_ROOT
export DESTDIR

install -d $DESTDIR/usr/bin
install -d $DESTDIR%{_mandir}/man1
install -d $DESTDIR/etc
install -d $DESTDIR%{SHAREDIR}
install -m644 nautilus.cfg $DESTDIR/etc/nautilus.cfg
install -m644 logon_new.v $DESTDIR%{SHAREDIR}
install -m644 ring_new.v $DESTDIR%{SHAREDIR}
make BINDIR=$DESTDIR/usr/bin MANDIR=$DESTDIR/%{_mandir} \
	SHAREDIR=$DESTDIR%{SHAREDIR} install

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root)
%doc Changelog doc/*
%{_mandir}/man1/*.1*
%{SHAREPACK}
%{SHAREDIR}
%config /etc/*
/usr/bin/*


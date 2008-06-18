%define name kadeploy
%define version 3.0.0
%define release 1

Summary: FIXME
URL: FIXME
Name: %{name}
Version: %{version}
Release: %{release}
Source0: http://FIXME/dist/%{name}-%{version}.tar.gz
License: GPL
Vendor: INRIA
Packager: Nicolas Niclausse
Group: Development/Tools
BuildArch: noarch
BuildRequires: erlang
Requires: erlang
BuildRoot: %{_tmppath}/%{name}-buildroot
Prefix: %{_prefix}

%description
 kadeploy is a FIXME .

%prep
%setup
%configure

make

%install
rm -rf $RPM_BUILD_ROOT
export DESTDIR=$RPM_BUILD_ROOT %makeinstall
install -m644 CONTRIBUTORS $RPM_BUILD_ROOT/usr/share/doc/%{name}/
install -m644 README $RPM_BUILD_ROOT/usr/share/doc/%{name}/
install -m644 TODO $RPM_BUILD_ROOT/usr/share/doc/%{name}/
install -m644 COPYING $RPM_BUILD_ROOT/usr/share/doc/%{name}/
install -m644 CHANGES $RPM_BUILD_ROOT/usr/share/doc/%{name}/

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root)
/usr/bin/kadeploy
/usr/lib/erlang/lib
/usr/lib/kadeploy
/usr/share/kadeploy
%doc /usr/share/doc/kadeploy/*
%doc /usr/share/man/man1/kadeploy.1.gz

%changelog

# end of file
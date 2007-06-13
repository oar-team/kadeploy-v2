
s/@/@@/g

/=/ {

	s/=head1/@subsection/

	s/=over/@table @option/

	s/=item/@item/

	s/=back/@end table/

}



s/I<\([^>]*\)>/@i{\1}/g

s/B<\([^>]*\)>/@b{\1}/g

s/L<\([^>]*\)(.)>/@ref{\1 man}/g

/^ [^ ].*$/{
:x
N
/^ [^\n]*\(\n [^\n]*\)*$/b x
s/^\(.*\)$/@example\
\1@end example\
/
}

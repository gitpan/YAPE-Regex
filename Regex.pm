package YAPE::Regex;

use YAPE::Regex::Element;
use Carp;
use strict;
use vars '$VERSION';

$VERSION = '1.02';


my %pat = (
  Pcomment => qr{ \( \? \# ( [^)]* ) \) }x,
  Xcomment => qr{ \# [^\S\n]* ( .* \n ) }x,
  Pflags => qr{ \( \? ([isxm]*)-?([ismx]*) \) }x,

  Pahead => qr{ \( \? ( [=!] ) }x,
  Pbehind => qr{ \( \? < ( [=!] ) }x,
  Pcond => qr{ \( \? \( (\d+) \) }x,
  Pcut => qr{ \( \? > }x,
  Pgroup => qr{ \( \? ([isxm]*)-?([ismx]*) : }x,
  Pcapture => qr{ \( (?! \? ) }x,
    Pclose => qr{ \) }x,

  quant => qr{ ( (?: [+*?] | \{ \d+ ,? \d* \} ) ) }x,
  ngreed => qr{ ( \? ) }x,

  anchor => qr{ ( \\ [ABbGZz] | [\^\$] ) }x,
  macro => qr{ \\ ( [dDwWsS] ) }x,
  oct => qr{ \\ ( [0-3] [0-7] [0-7] ) }x,
  hex => qr{ \\ x ( [a-fA-F0-9] [a-fA-F0-9] ) }x,
  backref => qr{ \\ ( [1-9] \d* ) }x,
  ctrl => qr{ \\ c ( . ) }x,
  slash => qr{ \\ ( . ) }x,
  any => qr{ \. }x,
  class => qr{ \[ ( \^? ) ( \]? [^]\\]* (?: \\. [^]\\]* )* ) \] }x,
  nws => qr{ ( (?: [^\s^\$|\\+*?()\[.]+ | \{ (?! \d+ ,? \d* \} ) )+ ) }x,
  reg => qr{ ( (?: [^^\$|\\+*?()\[.\{] | \{ (?! \d+ ,? \d* \} ) )+ ) }x,

  alt => qr{ \| }x,
);


sub import {
  shift;
  my @obj = qw(
    anchor macro oct hex backref ctrl slash any class text alt
    comment whitespace flags lookahead lookbehind conditional
    group capture close
  );
  no strict 'refs';
  for my $class ('YAPE::Regex', @_) {
    (my $file = $class . ".pm") =~ s!::!/!g;
    require $file and $class->import if not $INC{$file};
    if ($class ne 'YAPE::Regex') {
      push @{"${class}::ISA"}, 'YAPE::Regex';
      push @{"${class}::${_}::ISA"},
        "YAPE::Regex::$_", "${class}::Element" for @obj;
    }
    push @{"${class}::${_}::ISA"}, 'YAPE::Regex::Element' for @obj;
  }
} 


sub new {
  my ($class, $regex) = @_;

  croak "no regex given to $class->new"
    if not defined $regex or length($regex) == 0;

  eval { $regex = qr/$regex/ if ref($regex) ne 'Regexp' };

  $regex = "(?:$regex)" if $@;

  my $self = bless {
    TREE => [],
    TREE_STACK => [],
    CAPTURE => [],
    CONTENT => "$regex",
    DEPTH => 0,
  }, $class;
  $self->{CURRENT} = $self->{TREE};

  return $self;
}


sub state { $_[0]{STATE} }
sub error { $_[0]{ERROR} }
sub depth { $_[0]{DEPTH} }
sub chunk { substr $_[0]{CONTENT}, 0, $_[1] || 30 }
sub done { $_[0]{STATE} eq 'done' }
sub root { $_[0]{TREE}[0] }
sub top { $_[0]{TREE}[0] }
sub parse { 1 while $_[0]->next }
sub display { $_[0]->parse; $_[0]{TREE}[0]->fullstring if $_[0]->done;  }


sub next {
  my $self = shift;
  $self->{STATE} = 'done', return unless length $self->{CONTENT};

  if (
    @{$self->{TREE_STACK}} and
    $self->{TREE_STACK}[-1]{MODE}{x} and
    $self->{CONTENT} =~ s/^(\s+)//
  ) {
    my $node = (ref($self) . "::whitespace")->new($1);
    push @{ $self->{CURRENT} }, $node;
    return $node;
  }

  if ($self->{CONTENT} =~ s/^$pat{Pcomment}//) {
    my $node = (ref($self) . "::comment")->new($1,0);
    push @{ $self->{CURRENT} }, $node;
    $self->{STATE} = 'comment';
    return $node;
  }

  if (
    @{ $self->{TREE_STACK} } and
    $self->{TREE_STACK}[-1]{MODE}{x} and
    $self->{CONTENT} =~ s/^$pat{Xcomment}//
  ) {
    my $node = (ref($self) . "::comment")->new($1,1);
    push @{ $self->{CURRENT} }, $node;
    $self->{STATE} = 'comment';
    return $node;
  }

  if ($self->{CONTENT} =~ s/^$pat{anchor}//) {
    my $match = $1;
    my ($quant,$ngreed) = $self->_get_quant;
    return if $quant eq -1;
    my $node = (ref($self) . "::anchor")->new($match,$quant,$ngreed);
    push @{ $self->{CURRENT} }, $node;
    $self->{STATE} = 'anchor';
    return $node;
  }

  if ($self->{CONTENT} =~ s/^$pat{macro}//) {
    my $match = $1;
    my ($quant,$ngreed) = $self->_get_quant;
    return if $quant eq -1;
    my $node = (ref($self) . "::macro")->new($match,$quant,$ngreed);
    push @{ $self->{CURRENT} }, $node;
    $self->{STATE} = 'macro';
    return $node;
  }

  if ($self->{CONTENT} =~ s/^$pat{oct}//) {
    my $match = $1;
    my ($quant,$ngreed) = $self->_get_quant;
    return if $quant eq -1;
    my $node = (ref($self) . "::oct")->new($match,$quant,$ngreed);
    push @{ $self->{CURRENT} }, $node;
    $self->{STATE} = 'oct';
    return $node;
  }

  if ($self->{CONTENT} =~ s/^$pat{hex}//) {
    my $match = $1;
    my ($quant,$ngreed) = $self->_get_quant;
    my $node = (ref($self) . "::hex")->new($match,$quant,$ngreed);
    push @{ $self->{CURRENT} }, $node;
    $self->{STATE} = 'hex';
    return $node;
  }

  if ($self->{CONTENT} =~ s/^$pat{backref}//) {
    my $match = $1;
    my ($quant,$ngreed) = $self->_get_quant;
    return if $quant eq -1;
    my $node = (ref($self) . "::backref")->new($match,$quant,$ngreed);

  # this code is special for YAPE::Regex::Reverse
  if ($self->isa('YAPE::Regex::Reverse')) {
    if ($quant eq '*' or $quant eq '+') {
      $node = (ref($self) . "::group")->new;
      $node->{NGREED} = '?' if $quant eq '*';
      $node->{CONTENT} = [
        (ref($self) . "::backref")->new($match,'*'),
        (ref($self) . "::backref")->new($match),
      ];
    }
    elsif ($quant and $quant ne '?') {
      my ($l,$u) = $quant =~ /(\d+),(\d*)/;
      $node = (ref($self) . "::group")->new;
      $node->{NGREED} = '?' if not $l;
      $l-- if $l; $u-- if $u;
      $node->{CONTENT} = [
        (ref($self) . "::backref")->new($match,"{$l,$u}"),
        (ref($self) . "::backref")->new($match),
      ];
    }
  }
    push @{ $self->{CURRENT} }, $node;
    $self->{STATE} = 'backref';
    return $node;
  }

  if ($self->{CONTENT} =~ s/^$pat{ctrl}//) {
    my $match = $1;
    my ($quant,$ngreed) = $self->_get_quant;
    return if $quant eq -1;
    my $node = (ref($self) . "::ctrl")->new($match,$quant,$ngreed);
    push @{ $self->{CURRENT} }, $node;
    $self->{STATE} = 'ctrl';
    return $node;
  }

  if ($self->{CONTENT} =~ s/^$pat{slash}//) {
    my $match = $1;
    my ($quant,$ngreed) = $self->_get_quant;
    return if $quant eq -1;
    my $node = (ref($self) . "::slash")->new($match,$quant,$ngreed);
    push @{ $self->{CURRENT} }, $node;
    $self->{STATE} = 'slash';
    return $node;
  }

  if ($self->{CONTENT} =~ s/^$pat{any}//) {
    my ($quant,$ngreed) = $self->_get_quant;
    return if $quant eq -1;
    my $node = (ref($self) . "::any")->new($quant,$ngreed);
    push @{ $self->{CURRENT} }, $node;
    $self->{STATE} = 'any';
    return $node;
  }

  if ($self->{CONTENT} =~ s/^$pat{class}//) {
    my ($neg,$match) = ($1,$2);
    my ($quant,$ngreed) = $self->_get_quant;
    return if $quant eq -1;
    my $node = (ref($self) . "::class")->new($match,$neg,$quant,$ngreed);
    push @{ $self->{CURRENT} }, $node;
    $self->{STATE} = 'class';
    return $node;
  }

  if (
    @{ $self->{TREE_STACK} } and
    $self->{TREE_STACK}[-1]{MODE}{x} and
    $self->{CONTENT} =~ s/^$pat{nws}//
  ) {
    my $match = $1;
    my ($quant,$ngreed) = $self->_get_quant;
    my $node = (ref($self) . "::text")->new($match,$quant,$ngreed);
    push @{ $self->{CURRENT} }, $node;
    $self->{STATE} = 'text';
    return $node;
  }

  if ($self->{CONTENT} =~ s/^$pat{reg}//) {
    my $match = $1;
    my $node;
    if (length($match) > 1 and $self->{CONTENT} =~ /^$pat{quant}/) {
      $self->{CONTENT} = chop($match) . $self->{CONTENT};
      $node = (ref($self) . "::text")->new($match,"","");
    }
    else {
      my ($quant,$ngreed) = $self->_get_quant;
      return if $quant eq -1;
      $node = (ref($self) . "::text")->new($match,$quant,$ngreed);
    }      
    push @{ $self->{CURRENT} }, $node;
    $self->{STATE} = 'text';
    return $node;
  }

  if ($self->{CONTENT} =~ s/^$pat{alt}//) {{
    if (
      @{ $self->{TREE_STACK} } and
      $self->{TREE_STACK}[-1]->type eq 'cond' and
      $self->{TREE_STACK}[-1]{OPTS} == 2
    ) {
      $self->{CONTENT} = '|' . $self->{CONTENT};
      last;
    }
    my $node = (ref($self) . "::alt")->new;
    if (
      @{ $self->{TREE_STACK} } and
      $self->{TREE_STACK}[-1]->type eq 'cond'
    ) {
      $self->{TREE_STACK}[-1]{OPTS}++;
      $self->{CURRENT} = $self->{TREE_STACK}[-1]{FALSE};
    }
    else {
      push @{ $self->{CURRENT} }, $node;
    }
    $self->{STATE} = 'alt';
    return $node;
  }}

  if ($self->{CONTENT} =~ s/^$pat{Pflags}//) {
    my ($add,$sub) = ($1,$2);
    my $mode = $self->{TREE_STACK}[-1]{MODE};
    @{$mode}{split //, $add} = (1) x length($add);
    delete @{$mode}{split //, $sub};
    my $node = (ref($self) . "::flags")->new($add,$sub);
    push @{ $self->{CURRENT} }, $node;
    $self->{STATE} = 'flags';
    return $node;
  }

  if ($self->{CONTENT} =~ s/^$pat{Pcond}//) {
    my $node = (ref($self) . "::conditional")->new($1);
    $node->{MODE} = { %{ $self->{TREE_STACK}[-1]{MODE} } } if
      @{ $self->{TREE_STACK} };
    push @{ $self->{TREE_STACK} }, $node;
    push @{ $self->{CURRENT} }, $node;
    $self->{CURRENT} = $node->{TRUE};
    $self->{DEPTH}++;
    $self->{STATE} = "cond($1)";
    return $node;
  }

  if ($self->{CONTENT} =~ s/^$pat{Pahead}//) {
    my $node = (ref($self) . "::lookahead")->new($1 eq '=' ? 1 : 0);
    $node->{MODE} = { %{ $self->{TREE_STACK}[-1]{MODE} } };
    push @{ $self->{TREE_STACK} }, $node;
    push @{ $self->{CURRENT} }, $node;
    $self->{CURRENT} = $node->{CONTENT};
    $self->{DEPTH}++;
    $self->{STATE} = 'lookahead(' . ('neg','pos')[$1 eq '='] . ')';
    return $node;
  }

  if ($self->{CONTENT} =~ s/^$pat{Pbehind}//) {
    my $node = (ref($self) . "::lookbehind")->new($1 eq '=' ? 1 : 0);
    $node->{MODE} = { %{ $self->{TREE_STACK}[-1]{MODE} } };
    push @{ $self->{TREE_STACK} }, $node;
    push @{ $self->{CURRENT} }, $node;
    $self->{CURRENT} = $node->{CONTENT};
    $self->{DEPTH}++;
    $self->{STATE} = 'lookbehind(' . ('neg','pos')[$1 eq '='] . ')';
    return $node;
  }

  if ($self->{CONTENT} =~ s/^$pat{Pgroup}//) {
    my ($add,$sub) = ($1,$2);
    my $node = (ref($self) . "::group")->new($add || "",$sub || "");
    $node->{MODE} = { %{ $self->{TREE_STACK}[-1]{MODE} } } if
      @{ $self->{TREE_STACK} };
    @{$node->{MODE}}{split //, $add} = (1) x length($add);
    delete @{$node->{MODE}}{split //, $sub};
    push @{ $self->{TREE_STACK} }, $node;
    push @{ $self->{CURRENT} }, $node;
    $self->{CURRENT} = $node->{CONTENT};
    $self->{DEPTH}++;
    $self->{STATE} = 'group';
    return $node;
  }

  if ($self->{CONTENT} =~ s/^$pat{Pcapture}//) {
    my $node = (ref($self) . "::capture")->new;
    $node->{MODE} = { %{ $self->{TREE_STACK}[-1]{MODE} } } if
      @{ $self->{TREE_STACK} };
    push @{ $self->{TREE_STACK} }, $node;
    push @{ $self->{CURRENT} }, $node;
    push @{ $self->{CAPTURE} }, $node;
    $self->{CURRENT} = $node->{CONTENT};
    $self->{DEPTH}++;
    $self->{STATE} = 'capture(' . @{ $self->{CAPTURE} } . ')';
    return $node;
  }

  if ($self->{DEPTH}-- and $self->{CONTENT} =~ s/^$pat{Pclose}//) {
    my ($quant,$ngreed) = $self->_get_quant;
    return if $quant eq -1;
    my $node = (ref($self) . "::close")->new;
    $self->{CURRENT} = pop @{ $self->{TREE_STACK} };
    $self->{CURRENT}{QUANT} = $quant;
    $self->{CURRENT}{NGREED} = $ngreed;

  # this code is special to YAPE::Regex::Reverse
  if ($self->isa('YAPE::Regex::Reverse')) {
    if ($quant eq '*' or $quant eq '+') {
      my $old = $self->{CURRENT}{CONTENT};
      $self->{CURRENT}{CONTENT} = [
        (ref($self) . "::group")->new,
        (ref($self) . "::capture")->new,
      ];
      $self->{CURRENT}{NGREED} = '?' if $quant eq '*';
      $self->{CURRENT}{CONTENT}[0]{CONTENT} = $old;
      $self->{CURRENT}{CONTENT}[0]{QUANT} = '*';
      $self->{CURRENT}{CONTENT}[1]{CONTENT} = $old;
      $self->{CAPTURE}[-1] = $self->{CURRENT}{CONTENT}[1];
      bless $self->{CURRENT}, (ref($self) . '::group');
    }
    elsif ($quant and $quant ne '?') {
      my ($l,$u) = $quant =~ /(\d+),(\d*)/;
      my $old = $self->{CURRENT}{CONTENT};
      $self->{CURRENT}{CONTENT} = [
        (ref($self) . "::group")->new,
        (ref($self) . "::capture")->new,
      ];
      $self->{CURRENT}{NGREED} = '?' if not $l;
      $l-- if $l; $u-- if $u;
      $self->{CURRENT}{CONTENT}[0]{CONTENT} = $old;
      $self->{CURRENT}{CONTENT}[0]{QUANT} = "{$l,$u}";
      $self->{CURRENT}{CONTENT}[1]{CONTENT} = $old;
      $self->{CAPTURE}[-1] = $self->{CURRENT}{CONTENT}[1];
      bless $self->{CURRENT}, (ref($self) . '::group');
    }
  }

    $self->{CURRENT} = $self->{TREE_STACK}[-1];
    $self->{CURRENT} = $self->{CURRENT}{CONTENT};

    $self->{STATE} = 'close';
    return $node;
  }

  my $token = $self->chunk(1);
  $self->{ERROR} = "unexpected token '$token' during '$self->{STATE}'";
  $self->{STATE} = 'error';

  return;
}


sub extract {
  my $self = shift;
  $self->parse;
  
  my @nodes = @{ $self->{CAPTURE} };
  
  return sub { shift @nodes };
}


sub _get_quant {
  my $self = shift;
  my ($quant,$ngreed) = ("","");

  if (
    $self->{CONTENT} =~ s/^($pat{Pcomment})?$pat{quant}// or
    (@{ $self->{TREE_STACK} } and $self->{TREE_STACK}[-1]{MODE}{x} and
      $self->{CONTENT} =~ s/^($pat{Xcomment}\s*)?$pat{quant}//)
  ) {
    $quant = $+;
    {
      if ($quant =~ /^\{(\d+),(\d+)\}/ and $1 > $2) {
        $self->{ERROR} = "upper bound lower than lower bound ($quant)";
        $self->{STATE} = 'error';
        return -1;
      }
    }
    $self->{CONTENT} = $1 . $self->{CONTENT} if $1;
  }

  my ($ws) = $1 if
    @{ $self->{TREE_STACK} } and
    $self->{TREE_STACK}[-1]{MODE}{x} and
    $self->{CONTENT} =~ s/^(\s+)//;

  if (
    (@{ $self->{TREE_STACK} } and $self->{TREE_STACK}[-1]{MODE}{x} and
      $self->{CONTENT} =~ s/^($pat{Xcomment}\s*)?$pat{ngreed}//) or
      $self->{CONTENT} =~ s/^($pat{Pcomment})?$pat{ngreed}//
  ) {
    $ngreed = $+;
    $self->{CONTENT} = $1 . $self->{CONTENT} if $1;
  }

  $self->{CONTENT} = $ws . $self->{CONTENT} if $ws;

  return ($quant,$ngreed);
}


1;


__END__

=head1 NAME

YAPE::Regex - Yet Another Parser/Extractor for Regular Expressions

=head1 SYNOPSIS

  use YAPE::Regex;
  use strict;
  
  my $regex = qr/reg(ular\s+)?exp?(ression)?/i;
  my $parser = YAPE::Regex->new($regex);
  
  # here is the tokenizing part
  while (my $chunk = $parser->next) {
    # ...
  }

=head1 C<YAPE> MODULES

The C<YAPE> hierarchy of modules is an attempt at a unified means of parsing
and extracting content.  It attempts to maintain a generic interface, to
promote simplicity and reusability.  The API is powerful, yet simple.  The
modules do tokenization (which can be intercepted) and build trees, so that
extraction of specific nodes is doable.

=head1 DESCRIPTION

This module is yet another (?) parser and tree-builder for Perl regular
expressions.  It builds a tree out of a regex, but at the moment, the extent of
the extraction tool for the tree is quite limited (see L<Extracting Sections>).
However, the tree can be useful to extension modules.

=head1 USAGE

In addition to the base class, C<YAPE::Regex>, there is the auxiliary class
C<YAPE::Regex::Element> (common to all C<YAPE> base classes) that holds the
individual nodes' classes.  There is documentation for the node classes in
that module's documentation.

=head2 Methods for C<YAPE::Regex>

=over 4

=item * C<use YAPE::Regex;>

=item * C<use YAPE::Regex qw( MyExt::Mod );>

If supplied no arguments, the module is loaded normally, and the node classes
are given the proper inheritence (from C<YAPE::Regex::Element>).  If you supply
a module (or list of modules), C<import> will automatically include them (if
needed) and set up I<their> node classes with the proper inheritence -- that is,
it will append C<YAPE::Regex> to C<@MyExt::Mod::ISA>, and C<YAPE::Regex::xxx>
to each node class's C<@ISA> (where C<xxx> is the name of the specific node
class).

  package MyExt::Mod;
  use YAPE::Regex 'MyExt::Mod';
  
  # @MyExt::Mod::ISA = 'YAPE::Regex'
  # @MyExt::Mod::text::ISA = 'YAPE::Regex::text'
  # ...

=item * C<my $p = YAPE::Regex-E<gt>new($REx);>

Creates a C<YAPE::Regex> object, using the contents of C<$REx> as a regular
expression.  The C<new> method will I<attempt> to convert C<$REx> to a compiled
regex (using C<qr//>) if C<$REx> isn't already one.  If there is an error in
the regex, this will fail, but the parser will pretend it was ok.  It will then
report the bad token when it gets to it, in the course of parsing.

=item * C<my $text = $p-E<gt>chunk($len);>

Returns the next C<$len> characters in the input string; C<$len> defaults to
30 characters.  This is useful for figuring out why a parsing error occurs.

=item * C<my $done = $p-E<gt>done;>

Returns true if the parser is done with the input string, and false otherwise.

=item * C<my $errstr = $p-E<gt>error;>

Returns the parser error message.

=item * C<my $backref = $p-E<gt>extract;>

Returns a code reference that returns the next back-reference in the regex.
For more information on enhancements in upcoming versions of this module, check
L<Extracting Sections>.

=item * C<my $node = $p-E<gt>display(...);>

Returns a string representation of the entire content.  It calls the C<parse>
method in case there is more data that has not yet been parsed.  This calls the
C<fullstring> method on the root nodes.  Check the C<YAPE::Regex::Element> docs
on the arguments to C<fullstring>.

=item * C<my $node = $p-E<gt>next;>

Returns the next token, or C<undef> if there is no valid token.  There will be
an error message (accessible with the C<error> method) if there was a problem in
the parsing.

=item * C<my $node = $p-E<gt>parse;>

Calls C<next> until all the data has been parsed.

=item * C<my $node = $p-E<gt>root;>

Returns the root node of the tree structure.

=item * C<my $state = $p-E<gt>state;>

Returns the current state of the parser.  It is one of the following values:
C<alt>, C<anchor>, C<any>, C<backref>, C<capture(N)>, C<class>, C<close>,
C<comment>, C<cond(TYPE)>, C<ctrl>, C<done>, C<error>, C<flags>, C<group>,
C<hex>, C<macro>, C<lookahead(neg|pos)>, C<lookbehind(neg|pos)>, C<oct>,
C<slash>, and C<text>.

For C<capture(N)>, I<N> will be the number the captured pattern represents.

For C<cond(TYPE)>, I<TYPE> will either be a number representing the
back-reference that the conditional depends on, or one of the strings
C<lookahead(neg)>, C<lookahead(pos)>, C<lookbehind(neg)>, or C<lookbehind(pos)>.

For C<lookahead> and C<lookbehind>, one of C<neg> and C<pos> will be there,
depending on the type of assertion.

=item * C<my $node = $p-E<gt>top;>

Synonymous to C<root>.

=back

=head2 Extracting Sections

While extraction of nodes is the goal of the C<YAPE> modules, the author is at
a loss for words as to what needs to be extracted from a regex.  At the current
time, all the C<extract> method does is allow you access to the regex's set of
back-references:

  my $extor = $parser->extract;
  while (my $backref = $extor->()) {
    # ...
  }

C<japhy> is very open to suggestions as to the approach to node extraction (in
how the API should look, in addition to what should be proffered).  Preliminary
ideas include extraction keywords like the output of B<-Dr> (or the C<re>
module's C<debug> option).

The C<YAPE::Regex::Wasted> extension module, which suggests that regexes like
C</(.*?):/> be changed to C</([^:]*):/> (and their ilk), could make use of an
extraction technique that lets the user detect a node of C<.*?> followed by a
constant string or character class.

=head1 EXTENSIONS

=over 4

=item * C<YAPE::Regex::Explain> 1.01

Presents an explanation of a regular expression, node by node.

=item * C<YAPE::Regex::Reverse> (Not released)

Reverses the nodes of a regular expression.

=item * C<YAPE::Regex::Wasted> (Not released)

Points out wasted C</s> and C</m> modifiers, and tries to suggest replacements
for C<.*?> nodes.

=back

=head1 TO DO

This is a listing of things to add to future versions of this module.

=head2 API

=over 4

=item * Create a robust C<extract> method

Open to suggestions.

=item * Add support for C<(?(?=...))> and the like

Currently, only C<(?(NUMBER))> is supported for conditionals.

=back

=head2 Internals

=over 4

=item * Add Perl 5.6 character class support

The new character class syntaces, C<[:posix:]> and C<\p{UniCode}>, aren't yet
supported.  These might be C<class> objects, or have their own classes
(C<posix_class> and C<unicode_class>).

=head1 BUGS

Following is a list of known or reported bugs.

=head2 Pending

=over 4

=item * C<(?{ ... })> and C<(??{ ... })> not supported

=back

=head1 SUPPORT

Visit C<YAPE>'s web site at F<http://www.pobox.com/~japhy/YAPE/>.

=head1 SEE ALSO

The C<YAPE::Regex::Element> documentation, for information on the node classes.

=head1 AUTHOR

  Jeff "japhy" Pinyan
  CPAN ID: PINYAN
  japhy@pobox.com
  http://www.pobox.com/~japhy/

=cut

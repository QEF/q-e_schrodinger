# LaTeX2HTML 2021.2 (Released July 1, 2021)
# Associate labels original text with physical files.


$key = q/Sec:para/;
$external_labels{$key} = "$URL/" . q|node6.html|; 
$noresave{$key} = "$nosave";

$key = q/SubSec:Examples/;
$external_labels{$key} = "$URL/" . q|node5.html|; 
$noresave{$key} = "$nosave";

1;


# LaTeX2HTML 2021.2 (Released July 1, 2021)
# labels from external_latex_labels array.


$key = q/Sec:para/;
$external_latex_labels{$key} = q|4 Parallelism|; 
$noresave{$key} = "$nosave";

$key = q/SubSec:Examples/;
$external_latex_labels{$key} = q|3.1 Running examples|; 
$noresave{$key} = "$nosave";

1;


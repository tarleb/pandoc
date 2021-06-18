```
% pandoc -t opendocument+native_numbering

![First image](lalune.jpg)

![Second image](lalune.jpg)

^D
<text:p text:style-name="Text_20_body"><draw:frame draw:name="img1"><draw:image xlink:href="lalune.jpg" xlink:type="simple" xlink:show="embed" xlink:actuate="onLoad" /></draw:frame></text:p>
<text:p text:style-name="Text_20_body"><draw:frame draw:name="img2"><draw:image xlink:href="lalune.jpg" xlink:type="simple" xlink:show="embed" xlink:actuate="onLoad" /></draw:frame></text:p>
```

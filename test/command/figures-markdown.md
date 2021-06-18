Figure float with caption at the figure level.

```
% pandoc -f native -t markdown
[Figure ("fig-id",[],[]) (Caption Nothing [Para [Str "Caption"]]) [Para [Image ("",[],[]) [] ("foo.png", "fig:")]]]

^D
<figure id="fig-id">
<figure>
<img src="foo.png" />
</figure>
<figcaption><p>Caption</p></figcaption>
</figure>
```

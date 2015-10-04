### I've Created a Monster

### 2015-06-27

Today I wrote a static blog generator in ruby, and I've affectionately given it the name Blarg. It's basic workings when 
called without any arguments are as follows: 
Get a list of files from a directory &#8594; Get a list of files from the blog index &#8594; Compare the two &#8594; Update the blog index 
as necessary &#8594; Update the homepage with the latest blog post. 
When called with arguments it takes a template file and a markdown file then squishes them together resulting in a fresh 
new blog post. The post is then popped into the posts directory and the update function is run. Blarg also houses some 
ridiculous one-liners like `template = "<li>#{date} &raquo; <a href='posts/#{date}:#{subject.gsub("'", "&#39;")}'>#{subject.gsub(".html", "")}</a></li>\n"` 
It currently relies on the gems [nokogiri](http://www.nokogiri.org) and [redcarpet](https://github.com/vmg/redcarpet). If you 
hate yourself you can check it out [here](https://github.com/Peytonien/Blarg)

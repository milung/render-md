FROM swipl
COPY . /usr/bin/renderMD

ENV http_port 80
ENV base_url ''
ENV book_base /var/www/book

WORKDIR /usr/bin/renderMD

CMD ["swipl", "--quiet", "start.pl"]

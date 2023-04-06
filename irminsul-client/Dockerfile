FROM node:18-alpine AS build

WORKDIR /app
ADD package.json /app/package.json
ADD package-lock.json /app/package-lock.json

RUN npm ci

ADD . /app

RUN npm run build

FROM dragas/thttpd:latest

EXPOSE 80
COPY --from=build /app/build /var/www/http

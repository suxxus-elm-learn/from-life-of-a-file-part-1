FROM node:16-alpine3.15

WORKDIR "/app"

RUN npm install create-elm-app -g

RUN create-elm-app elm-app

RUN cd elm-app && ls -a

EXPOSE 3000
WORKDIR "/app/elm-app"
CMD ["elm-app", "start"]

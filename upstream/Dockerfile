FROM mcr.microsoft.com/dotnet/aspnet:10.0-alpine

RUN apk add --no-cache icu-libs
ENV DOTNET_SYSTEM_GLOBALIZATION_INVARIANT=false
RUN apk add tzdata

COPY server/bin/Release/net10.0/publish/ app/
WORKDIR /app
ENTRYPOINT ["dotnet", "Fire.dll"]

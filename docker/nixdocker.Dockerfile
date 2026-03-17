
FROM docker.io/nixos/nix:latest AS nix-dev-base 
RUN nix-channel --update
RUN printf '%s\n' "extra-experimental-features = nix-command flakes" "extra-trusted-users = $USER" | tee -a /etc/nix/nix.conf 
RUN  nix profile install --impure 'github:katrinafyi/pac-nix#aslp'  
RUN  nix profile install --impure 'github:katrinafyi/pac-nix#bap-aslp'  
RUN  nix profile install --impure 'nixpkgs#boogie'
RUN  nix profile install --impure 'nixpkgs#openjdk'
RUN  nix profile install --impure 'nixpkgs#coursier' && cs setup --yes
RUN  cs install mill
ENV  PATH=$PATH:/root/.local/share/coursier/bin


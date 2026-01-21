# AND pattern filtering

Setup test files:

    $ mkdir -p src
    $ cat << 'EOF' > src/api.js
    > function getUser(id) {
    >   return fetch('/api/user/' + id);
    > }
    > function updateUser(id, data) {
    >   return fetch('/api/user/' + id, {method: 'POST', body: data});
    > }
    > EOF
    $ cat << 'EOF' > src/utils.js
    > function formatDate(d) {
    >   return d.toISOString();
    > }
    > EOF
    $ cat << 'EOF' > src/auth.js
    > function login(user, pass) {
    >   return fetch('/api/auth', {body: {user, pass}});
    > }
    > EOF

Basic AND - file must contain both patterns, shows all matches from both:

    $ zg fetch -a getUser src --color never --sort
    src/api.js:1:function getUser(id) {
    src/api.js:2:  return fetch('/api/user/' + id);
    src/api.js:5:  return fetch('/api/user/' + id, {method: 'POST', body: data});

File excluded if missing any pattern (auth.js has fetch but no getUser):

    $ zg fetch -a getUser src --color never 2>&1 | grep -c auth.js || true
    0

Same line matching multiple patterns shown once:

    $ mkdir single
    $ cat << 'EOF' > single/both.js
    > fetchUser();
    > EOF
    $ zg fetch -a User single --color never
    single/both.js:1:fetchUser();

Multiple AND patterns:

    $ zg fetch -a getUser -a updateUser src --color never --sort
    src/api.js:1:function getUser(id) {
    src/api.js:2:  return fetch('/api/user/' + id);
    src/api.js:4:function updateUser(id, data) {
    src/api.js:5:  return fetch('/api/user/' + id, {method: 'POST', body: data});

Replace only affects last pattern:

    $ zg fetch -a getUser -r getData src -n --color never --sort
    ── src/api.js:1 ──
    - function getUser(id) {
    + function getData(id) {
    1 files, 1 replacements

No match if file missing any pattern:

    $ zg fetch -a nonexistent src --color never
    [1]

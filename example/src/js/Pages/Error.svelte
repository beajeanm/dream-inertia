<script>
export let status;
export let debug_info;

let titles = new Map([
  [503, "503: Service Unavailable"],
  [500, "500: Server Error"],
  [404, "404: Page Not Found"],
  [403, "403: Forbidden"],
]);

let title = titles.get(status);

let descriptions = new Map([
  [503, "Sorry, we are doing some maintenance. Please check back soon."],
  [500, "Whoops, something went wrong on our servers."],
  [404, "Sorry, the page you are looking for could not be found."],
  [403, "Sorry, you are forbidden from accessing this page."],
]);

let description = descriptions.get(status);
</script>

<div class="hero is-centered">
  <h1 class="title is-1 has-text-centered has-text-danger">{title}</h1>
  <div class="title is-4 has-text-centered">{description}</div>
  <pre class="content">{debug_info}</pre>
</div>

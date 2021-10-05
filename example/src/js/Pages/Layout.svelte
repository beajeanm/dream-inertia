<style>
</style>

<script lang="ts">
import { Link } from "@inertiajs/inertia-svelte";
import { onMount } from "svelte";
onMount(async () => {
  const burger = document.querySelector("#burger");
  if (burger instanceof HTMLElement) {
    const menu = document.querySelector("#" + burger.dataset.target);
    burger.addEventListener("click", function () {
      burger.classList.toggle("is-active");
      menu.classList.toggle("is-active");
    });
  }
});
</script>

<div class="section">
  <nav class="navbar" role="navigation" aria-label="main navigation">
    <div class="navbar-brand">
      <Link class="navbar-item" href="https://github.com/beajeanm/dream-inertia">
        <img alt="brand" src="https://svelte.dev/svelte-logo-horizontal.svg" width="73" height="73" />
      </Link>

      <button
        id="burger"
        class="navbar-burger"
        aria-label="menu"
        aria-expanded="false"
        data-target="navbarBasicExample">
        <span aria-hidden="true"></span>
        <span aria-hidden="true"></span>
        <span aria-hidden="true"></span>
      </button>
    </div>

    <div id="navbarBasicExample" class="navbar-menu">
      <div class="navbar-start">
        <Link href="/" class="navbar-item bd-navbar-item bd-navbar-item-base">
          Home
        </Link>
        <Link href="/" class="navbar-item">Documentation</Link>
      </div>

      <div class="navbar-end">
        <div class="navbar-item">
          <Link href="/signup" class="button is-primary">
            <strong>Sign up</strong>
          </Link>
        </div>
        <div class="navbar-item">
          <Link href="/login" class="button is-light">Log in</Link>
        </div>
      </div>
    </div>
  </nav>
  <main class="hero">
    <slot />
  </main>
</div>

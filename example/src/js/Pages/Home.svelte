<style>
</style>

<script lang="ts">
export let name: string;

import { Link } from "@inertiajs/inertia-svelte";
import Layout from "./Layout.svelte";
</script>

<Layout>
  <div class="hero-body">
    <div class="container has-text-centred is-centered">
      <h1 class="title is-capitalized has-text-primary">Hello {name}!</h1>
      <p>
        Visit the <Link href="https://svelte.dev/tutorial"
          >Svelte tutorial</Link> to learn how to build Svelte apps.
      </p>
      <div></div>
    </div>
  </div>
</Layout>

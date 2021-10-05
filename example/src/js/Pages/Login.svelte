<style>
</style>

<script>
import { Inertia } from "@inertiajs/inertia";
import Layout from "./Layout.svelte";

export let values = {
  username: null,
  password: null,
};

export let csrf = {
  token: null,
};

export let errors = {};

function handleSubmit() {
  Inertia.post("/login", {
    ...values,
    ...csrf,
  });
}
</script>

<Layout>
  {#if errors.general}<div class="content has-text-danger">
      {errors.general}
    </div>{/if}
  <div class="columns">
    <div class="column is-half is-centered">
      <form on:submit|preventDefault="{handleSubmit}">
        <div class="field">
          <label for="username" class="label">Username</label>
          <div class="control">
            <input
              id="username"
              class="input"
              type="text"
              placeholder="username"
              bind:value="{values.username}" />
            {#if errors.username}<div class="content has-text-danger">
                {errors.username}
              </div>{/if}
          </div>
        </div>

        <div class="field">
          <label for="password" class="label">Password</label>
          <div class="control">
            <input
              id="password"
              class="input"
              type="password"
              placeholder="password"
              bind:value="{values.password}" />
          </div>
          {#if errors.password}<div class="content has-text-danger">
              {errors.password}
            </div>{/if}
        </div>

        <div class="field">
          <div class="control">
            <button type="submit" class="button is-link">Submit</button>
          </div>
        </div>

        <input id="csrf_token" type="hidden" bind:value="{csrf.token}" />
      </form>
    </div>
  </div>
</Layout>

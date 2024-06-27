<script setup>
import { Link } from "@inertiajs/vue3";

defineProps({ counter: Number, message: String });
</script>

<template>
  <main>
    <div class="container">
      <div class="container">
        <h1>{{ message }} {{ counter }}</h1>
      </div>
      <div class="btn-group">
        <Link href="/count" :only="['counter', 'other']" as="button" type="button" class="btn btn-primary">Click here
        </Link>
        <Link href="/users" as="button" type="button" class="btn btn-primary">Users</Link>
        <Link href="/error" as="button" type="button" class="btn btn-danger">Error</Link>
      </div>
    </div>
  </main>
</template>

<style scoped></style>

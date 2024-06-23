<script setup>
import { reactive } from "vue";
import { Link, useForm } from "@inertiajs/vue3";
import TableComponent from "../components/Table.vue";

const headers = reactive(["first_name", "last_name", "email"]);

defineProps({
  users: Array,
});

const form = useForm({
  first_name: null,
  last_name: null,
  email: null,
});

function submit() {
  form.post("/users", {
    preserveScroll: true,
    onSuccess: () => form.reset(),
  });
}
</script>

<template>
  <h1>Users:</h1>
  <br />

  <div>
    <TableComponent :headers="headers" :data="users">
      <template #column0="{ entity }">
        {{ entity.first_name }}
      </template>
      <template #column1="{ entity }">
        {{ entity.last_name }}
      </template>
      <template #column2="{ entity }">
        {{ entity.email }}
      </template>
    </TableComponent>
  </div>
  <br />

  <div>
    <form @submit.prevent="submit">
      <label for="first_name">First name:</label>
      <input id="first_name" v-model="form.first_name" />
      <label for="last_name">Last name:</label>
      <input id="last_name" v-model="form.last_name" />
      <label for="email">Email:</label>
      <input id="email" v-model="form.email" />
      <button type="submit">Submit</button>
    </form>
  </div>
</template>

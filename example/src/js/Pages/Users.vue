<script setup>
import { reactive } from "vue";
import { Link, useForm } from "@inertiajs/vue3";
import TableComponent from "../components/Table.vue";

const headers = reactive(["first_name", "last_name", "email"]);

defineProps({
  users: Array,
  errors: Object,
});

const form = useForm({
  first_name: null,
  last_name: null,
  email: null,
});

function submit() {
  form.post("/users", {
    preserveScroll: true,
    preserveState: "errors",
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
      <div v-if="errors.first_name">{{ errors.first_name }}</div>
      <label for="last_name">Last name:</label>
      <input id="last_name" v-model="form.last_name" />
      <div v-if="errors.last_name">{{ errors.last_name }}</div>
      <label for="email">Email:</label>
      <input id="email" v-model="form.email" />
      <div v-if="errors.email">{{ errors.email }}</div>
      <button type="submit">Submit</button>
    </form>
  </div>
</template>

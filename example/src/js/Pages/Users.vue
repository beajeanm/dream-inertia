<script setup>
import { reactive } from "vue";
import { Link, useForm } from "@inertiajs/vue3";
import TableComponent from "../components/Table.vue";

const headers = reactive(["First Name", "Last Name", "Email"]);

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
  <h3>Current users:</h3>
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
  <h3>Add user:</h3>
  <div>
    <form @submit.prevent="submit" class="row g-3">
      <div class="col-mb-4">
        <label for="first_name" class="form-label">First name:</label>
        <input id="first_name" v-model="form.first_name" class="form-control"
          :class="{ ['is-invalid']: errors.first_name }" aria-describedby="firstNameFeedback" />
        <div id="firstNameFeedback" v-if="errors.first_name" class="invalid-feedback">
          {{ errors.first_name }}
        </div>
      </div>
      <div class="col-mb-4">
        <label for="last_name" class="form-label">Last name:</label>
        <input id="last_name" v-model="form.last_name" class="form-control"
          :class="{ ['is-invalid']: errors.last_name }" aria-describedby="lastNameFeedback" />
        <div id="lastNameFeedback" v-if="errors.last_name" class="invalid-feedback">
          {{ errors.last_name }}
        </div>
      </div>
      <div class="col-mb-4">
        <label for="email" class="form-label">Email:</label>
        <input id="email" v-model="form.email" class="form-control" :class="{ ['is-invalid']: errors.email }"
          aria-describedby="emailFeedback" />
        <div id="emailFeedback" v-if="errors.email" class="invalid-feedback">
          {{ errors.email }}
        </div>
      </div>
      <div class="col-12">
        <button type="submit" class="btn btn-primary">Submit</button>
      </div>
    </form>
  </div>
</template>

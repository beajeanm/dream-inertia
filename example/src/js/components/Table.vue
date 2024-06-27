<script setup>
defineProps({
  headers: {
    type: Array,
    required: true,
  },
  data: {
    type: Array,
    required: true,
  },
});
</script>

<template>
  <div>
    <table class="table">
      <thead>
        <tr>
          <th v-for="(header, i) in headers" :key="`${header}${i}`" class="header-item" scope="col">
            {{ header }}
          </th>
        </tr>
      </thead>
      <tbody>
        <tr v-for="entity in data" :key="`entity-${entity.name}`" class="table-rows">
          <td v-for="(header, i) in headers" :key="`${header}-${i}`">
            <slot :name="`column${i}`" :entity="entity"></slot>
          </td>
        </tr>
      </tbody>
    </table>
  </div>
</template>

import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { CanDeactivateGuard } from 'src/app/@data/guards/can-deactivate.guard';
import { Ley276Component } from './ley276.component';

const routes: Routes = [
  {
    path: '',
    component: Ley276Component,
    data: { title: 'Otras leyes | Servir Talento Perú' },
    canDeactivate: [CanDeactivateGuard],
  },
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class Ley276RoutingModule {}

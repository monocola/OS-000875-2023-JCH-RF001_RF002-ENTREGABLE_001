import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { CanDeactivateGuard } from 'src/app/@data/guards/can-deactivate.guard';
import { Ley1401Component } from './ley1401.component';

const routes: Routes = [
  {
    path: '',
    component: Ley1401Component,
    data: { title: 'Ley 1401 | Servir Talento Perú' },
    canDeactivate: [CanDeactivateGuard],
  },
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class Ley1401RoutingModule {}

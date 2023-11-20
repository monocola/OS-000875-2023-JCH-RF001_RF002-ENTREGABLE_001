import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { CanDeactivateGuard } from 'src/app/@data/guards/can-deactivate.guard';
import { Ley30057Component } from './ley30057.component';

const routes: Routes = [
  {
    path: '',
    component: Ley30057Component,
    data: { title: 'Ley 30057 | Servir Talento Perú' },
    canDeactivate: [CanDeactivateGuard],
  },
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class Ley30057RoutingModule {}

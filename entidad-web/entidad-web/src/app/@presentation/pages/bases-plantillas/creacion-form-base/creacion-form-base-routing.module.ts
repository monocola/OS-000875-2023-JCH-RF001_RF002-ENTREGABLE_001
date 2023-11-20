import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { BonificacionComponent } from './bonificacion/bonificacion.component';
import { CreacionFormBaseComponent } from './creacion-form-base.component';
import { LegalComponent } from './legal/legal.component';

const routes: Routes = [
  {
    path: '',
    component: CreacionFormBaseComponent,
    children: [
      { path: 'base-legal', component: LegalComponent },
      { path: 'bonificaciones', component: BonificacionComponent },
    ],
  },
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class CreacionFormBaseRoutingModule {}

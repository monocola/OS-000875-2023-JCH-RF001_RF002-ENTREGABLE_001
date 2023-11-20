import { ModalElegirPlantillaComponent } from './modal-elegir-plantilla/modal-elegir-plantilla.component';
import { ConfiguracionRequisitosMinComponent } from './configuracion-requisitos-min/configuracion-requisitos-min.component';
import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { EvaluacionConfiguracionComponent } from './evaluacion-configuracion.component';

const routes: Routes = [
  {
    path: '',
    component: EvaluacionConfiguracionComponent,
  },
  {
    path: 'configuracion-requisitos-minimos',
    component: ConfiguracionRequisitosMinComponent,
  },
  {
    path: 'modal-elegir-plantilla',
    component: ModalElegirPlantillaComponent,
  }
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class EvaluacionConfiguracionRoutingModule {}

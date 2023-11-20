import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { BasesPlantillasComponent } from './bases-plantillas.component';
import { CreacionPlantillasComponent } from './creacion-plantillas/creacion-plantillas.component';

const routes: Routes = [
  {
    path: '',
    component: BasesPlantillasComponent,
  },
  {
    path: 'creacion',
    component: CreacionPlantillasComponent,
    data: {
      title: 'Selecciona el tipo de informe | Servir Talento Perú',
    },
  },
  {
    path: '',
    children: [
      {
        path: '',
        loadChildren: () =>
          import('./creacion-form-base/creacion-form-base.module').then(
            (m) => m.CreacionFormBaseModule
          ),
      },
    ],
  },
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class BasesPlantillasRoutingModule {}

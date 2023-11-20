import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { NotFoundComponent } from '../pages/miscellaneous/not-found/not-found.component';
import { ExtraComponent } from './extra.component';
import { SolicitudComponent } from './solicitud/solicitud.component';
import { SolicitudJefeOrhComponent } from './solicitud-jefe-orh/solicitud-jefe-orh.component';
import { SolicitudEditarComponent } from './solicitud-editar/solicitud-editar.component';

const routes: Routes = [
  {
    path: '',
    component: ExtraComponent,
    children: [
      {
        path: 'solicitud',
        component: SolicitudComponent,
      }
    ],
  },
  {
    path: '',
    component: ExtraComponent,
    children: [
      {
        path: 'solicitud2',
        component: SolicitudJefeOrhComponent,
      }
    ],
  },
  {
    path: '',
    component: ExtraComponent,
    children: [
      { 
        path: 'solicitud3',
        component: SolicitudEditarComponent,
      }
    ],
  },
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class ExtraRoutingModule {}

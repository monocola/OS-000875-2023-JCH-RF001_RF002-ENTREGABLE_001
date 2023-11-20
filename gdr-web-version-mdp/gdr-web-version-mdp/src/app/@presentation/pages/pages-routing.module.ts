import { RouterModule, Routes } from '@angular/router';
import { NgModule } from '@angular/core';
import { PagesComponent } from './pages.component';
import { DefaultPageComponent } from './default-page/default-page.component';
import { NotFoundComponent } from './miscellaneous/not-found/not-found.component';
import { AuthGuard } from '../../@data/interceptors';
import { RolGuard } from '../../@data/interceptors/rol.guard';
import { HistorialReunionesEvaluadoComponent } from './reuniones-historial/historial-reuniones-evaluado/historial-reuniones-evaluado.component';
import { ImplementacionComponent } from './implementacion/implementacion.component';
import { AsignarGestorGdrComponent } from './asignar-entidad/asignar-gestor-gdr.component';

const routes: Routes = [
  {
    path: '',
    component: PagesComponent,
    /* canActivateChild: [RolGuard], */
    children: [
      {
        path: 'home',
        component: DefaultPageComponent,
        data: {
          title: 'Home',
        },
      },
      {
        path: 'entidad',
        data: {
          title: 'Entidad',
        },
        loadChildren: () =>
        import('./entidad/entidad.module').then(
          (m) => m.EntidadModule
        ),
      },
      {
        path: 'organigrama',
        data: {
          title: 'Gestión de organigrama - Servir GDR Perú',
        },
        loadChildren: () =>
          import('./organigrama/organigrama.module').then(
            (m) => m.OrganigramaModule
          ),
      },
      {
        path: 'servidores',
        data: {
          title: 'Servidores Civiles - Servir GDR Perú',
        },
        loadChildren: () =>
          import('./servidores/servidores.module').then(
            (m) => m.ServidoresModule
          ),
      },
      {
        path: 'ciclos',
        data: {
          title: 'Gestión de ciclos - Servir GDR Perú',
        },
        loadChildren: () =>
            import('./ciclos/ciclos.module').then(
            (m) => m.CiclosModule
          ),
      },
      {
        path: 'profile',
        data: {
          title: 'Datos de Usuario - Servir GDR Perú',
        },
        loadChildren: () =>
          import('./profile/profile.module').then(
            (m) => m.ProfileModule
          ),
      },
      {
        path: 'participantes',
        data: {
          title: 'Participantes - Servir GDR Perú',
        },
        loadChildren: () =>
          import('./participantes/participantes.module').then(
            (m) => m.ParticipantesModule
          ),
      },
      {
        path: 'evaluados',
        data: {
          title: 'Evaluados - Servir GDR Perú',
        },
        loadChildren: () =>
          import('./evaluados/evaluados.module').then(
            (m) => m.EvaluadosModule
          ),
      },
      {
        path: 'factoresEvaluacion',
        data: {
          title: 'Factores de Evaluación - Servir GDR Perú',
        },
        loadChildren: () =>
          import('./factores-evaluacion/factores-evaluacion.module').then(
            (m) => m.FactoresEvaluacionModule
          ),
      },
      {
        path: 'cronograma',
        data: {
          title: 'Cronograma - Servir GDR Perú',
        },
        loadChildren: () =>
          import('./cronograma/cronograma.module').then(
            (m) => m.CronogramaModule
          ),
      },
      {
        path: 'reuniones',
        data: {
          title: 'Reuniones - Servir GDR Perú',
        },
        loadChildren: () =>
          import('./reuniones/reuniones.module').then(
            (m) => m.ReunionesModule
          ),
      },
      {
        path: 'reuniones/historial',
        data: {
          title: 'Reuniones - Servir GDR Perú',
        },
        loadChildren: () =>
          import('./reuniones-historial/reuniones-historial.module').then(
            (m) => m.ReunionesHistorialModule
          ),
      },
      {
        path: 'implementacion',
        data: {
          title: 'Implementacion - Servir GDR Perú',
        },
        component: ImplementacionComponent,
        loadChildren: () =>
          import('./implementacion/implementacion.module').then(
            (m) => m.ImplementacionModule
          ),
      },
      {
        path: 'gestor-gdr/:entidadId',
        data: {
          title: 'Asignar Gestor Gdr - Servir GDR Perú',
        },
        component: AsignarGestorGdrComponent,
        loadChildren: () =>
          import('./asignar-entidad/asignar-gestor-gdr.module').then(
            (m) => m.AsignarGestorModule
          ),
      },
      {
        path: 'configuracion',
        data: {
          title: 'Configuracion - Servir GDR Perú',
        },
        loadChildren: () =>
          import('./configuracion/configuracion.module').then(
            (m) => m.ConfiguracionModule
          ),
      },
      {
        path: '',
        redirectTo: 'home',
        pathMatch: 'full',
      },
      {
        path: '**',
        data: {
          title: '404 No encontrado👻 - Servir GDR Perú',
        },
        component: NotFoundComponent,
      },
    ],
  },
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class PagesRoutingModule {
  menu = [];

  constructor() {}
}

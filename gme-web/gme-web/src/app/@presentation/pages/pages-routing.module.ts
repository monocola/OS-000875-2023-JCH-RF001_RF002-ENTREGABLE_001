import { RouterModule, Routes } from '@angular/router';
import { NgModule } from '@angular/core';
import { PagesComponent } from './pages.component';
import { DefaultPageComponent } from './default-page/default-page.component';
import { NotFoundComponent } from './miscellaneous/not-found/not-found.component';

const routes: Routes = [
  {
    path: '',
    component: PagesComponent,
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
          title: 'Gestión de organigrama - Servir Perú',
        },
        loadChildren: () =>
          import('./organigrama/organigrama.module').then(
            (m) => m.OrganigramaModule
          ),
      },
      {
        path: 'servidoresCiviles',
        data: {
          title: 'Servidores Civiles - Servir Perú',
        },
        loadChildren: () =>
          import('./servidores/servidores.module').then(
            (m) => m.ServidoresModule
          ),
      },
      {
        path: 'profile',
        data: {
          title: 'Datos de Usuario - Servir SGME Perú',
        },
        loadChildren: () =>
          import('./profile/profile.module').then(
            (m) => m.ProfileModule
          ),
      },
      {
        path: 'puestos',
        data: {
          title: 'Puestos',
        },
        loadChildren: () =>
        import('./puestos/puestos.module').then(
          (m) => m.PuestosModule
        ),
      },
      {
        path: 'gestoresOrh',
        data: {
          title: 'Gestion de Gestores ORH',
        },
        loadChildren: () =>
        import('./gestores-orh/gestores-orh.module').then(
          (m) => m.GestoresOrhModule
        ),
      },
      {
        path: 'solicitudesExt',
        data: {
          title: 'Solicitudes Externas - Servir SGME Perú',
        },
        loadChildren: () =>
          import('./solicitud-ext/solicitud-ext.module').then(
            (m) => m.SolicitudExtModule
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
          title: '404 No encontrado👻 - Servir Gestión Maestras Entidad',
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

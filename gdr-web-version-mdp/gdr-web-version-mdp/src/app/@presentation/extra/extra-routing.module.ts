import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { NotFoundComponent } from '../pages/miscellaneous/not-found/not-found.component';
import { ExtraComponent } from './extra.component';

const routes: Routes = [
  {
    path: '',
    component: ExtraComponent,
    children: [
      {
        path: 'register',
        data: {
          title: 'Registro - GDR Perú',
        },
        loadChildren: () =>
          import('./administrator/administrator.module').then(
            (m) => m.AdministratorModule
          ),
      },
      {
        path: 'actualiza',
        data: {
          title: 'Correción de datos - GDR Perú',
        },
        loadChildren: () =>
          import('./administrator/administrator.module').then(
            (m) => m.AdministratorModule
          ),
      },
      {
        path: '',
        redirectTo: 'administrator',
        pathMatch: 'full',
      },
      {
        path: '**',
        component: NotFoundComponent,
      },
    ],
  },
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class ExtraRoutingModule {}

import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { CreacionComponent } from './creacion/creacion.component';
import { PerfilesComponent } from './perfiles.component';
import { LazyLoadedRootComponent } from './helperComponent';

const routes: Routes = [
  {
    path: '',
    component: LazyLoadedRootComponent,
    children: [
      { path: '', component: PerfilesComponent },
      {
        path: 'creacion',
        component: CreacionComponent,
        data: { title: 'Creación de Perfil - Servir Talento Perú' },
      },
      {
        path: 'creacion/ley30057',
        loadChildren: () =>
          import('./ley30057/ley30057.module').then(
            (m) => m.Ley30057Module
          ),
      },
      {
        path: 'creacion/ley1401',
        loadChildren: () =>
          import('./ley1401/ley1401.module').then(
            (m) => m.Ley1401Module
          ),
      },
      {
        path: 'creacion/otras',
        loadChildren: () =>
          import('./ley276/ley276.module').then(
            (m) => m.Ley276Module
          ),
      },
    ],
  },
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class PerfilRoutingModule {}

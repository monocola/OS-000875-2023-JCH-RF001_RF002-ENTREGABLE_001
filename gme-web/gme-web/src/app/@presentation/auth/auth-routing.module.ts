import { RouterModule, Routes } from '@angular/router';
import { NgModule } from '@angular/core';

import { AuthComponent } from './auth.component';
import { LoginComponent } from './login/login.component';
import { ChangePasswordComponent } from './change-password/change-password.component';
import { ForgotPasswordComponent } from './forgot-password/forgot-password.component';

const routes: Routes = [
  {
    path: '',
    component: AuthComponent,
    children: [
      {
        path: 'login',
        data: {
          title: 'Login - Servir Gestión Maestras Entidad',
        },
        component: LoginComponent,
      },
      {
        path: 'change-password',
        data: {
          title: 'Cambiar contraseña - Servir Gestión Maestras Entidad',
        },
        component: ChangePasswordComponent,
      },
      {
        path: 'forgot-password',
        data: {
          title: 'Olvidé mi contraseña - Servir Gestión Maestras Entidad',
        },
        component: ForgotPasswordComponent,
      },
      {
        path: '',
        redirectTo: 'login',
        pathMatch: 'full',
      },
      {
        path: '**',
        redirectTo: 'login',
        pathMatch: 'full',
      },
    ],
  },
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class AuthRoutingModule {}

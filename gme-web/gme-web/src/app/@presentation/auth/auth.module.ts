import { NgModule } from '@angular/core';
import {
  NbActionsModule,
  NbButtonModule,
  NbCardModule, NbDialogModule,
  NbFormFieldModule,
  NbIconModule,
  NbInputModule,
  NbLayoutModule,
  NbMenuModule, NbOptionModule, NbSelectModule,
  NbSidebarModule,
  NbToggleModule
} from '@nebular/theme';

import { ThemeModule } from '../@theme/theme.module';
import { AuthComponent } from './auth.component';
import { AuthRoutingModule } from './auth-routing.module';
import { LoginComponent } from './login/login.component';
import { MatIconModule } from '@angular/material/icon';
import { ReactiveFormsModule } from '@angular/forms';
import { ForgotPasswordComponent } from './forgot-password/forgot-password.component';
import { ChangePasswordComponent } from './change-password/change-password.component';
import { CommonComponentsModule } from '../@common-components/common-components.module';
import { EntidadComponent } from './entidad/entidad.component';
import { MatDialogModule } from '@angular/material/dialog';
import { PerfilComponent } from './perfil/perfil.component';
import { MatCardModule } from '@angular/material/card';

@NgModule({
  imports: [
    AuthRoutingModule,
    ThemeModule,
    NbMenuModule,
    NbLayoutModule,
    NbSidebarModule,
    NbButtonModule,
    NbFormFieldModule,
    MatIconModule,
    NbInputModule,
    ReactiveFormsModule,
    NbIconModule,
    NbCardModule,
    NbToggleModule,
    CommonComponentsModule,
    NbActionsModule,
    NbOptionModule,
    NbDialogModule,
    MatDialogModule,
    NbSelectModule,
    MatCardModule
  ],
  declarations: [
    AuthComponent,
    LoginComponent,
    ForgotPasswordComponent,
    ChangePasswordComponent,
    EntidadComponent,
    PerfilComponent,
  ],
})
export class AuthModule {}

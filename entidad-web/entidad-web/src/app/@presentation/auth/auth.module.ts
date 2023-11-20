import { NgModule } from '@angular/core';
import {
  NbButtonModule,
  NbCardModule,
  NbFormFieldModule,
  NbIconModule,
  NbInputModule,
  NbLayoutModule,
  NbMenuModule,
  NbSidebarModule,
  NbToggleModule,
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
    CommonComponentsModule
  ],
  declarations: [
    AuthComponent,
    LoginComponent,
    ForgotPasswordComponent,
    ChangePasswordComponent,
  ],
})
export class AuthModule {}

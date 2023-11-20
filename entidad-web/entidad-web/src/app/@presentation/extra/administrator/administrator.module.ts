import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { AdministratorRoutingModule } from './administrator-routing.module';
import { AdministratorComponent } from './administrator.component';

import {
  NbAutocompleteModule,
  NbButtonModule,
  NbCardModule,
  NbDatepickerModule,
  NbFormFieldModule,
  NbIconModule,
  NbInputModule,
  NbLayoutModule,
  NbOptionModule,
  NbPopoverModule,
  NbSelectModule,
  NbSidebarModule,
  NbThemeModule,
} from '@nebular/theme';
import { RouterModule } from '@angular/router';
import { ReactiveFormsModule, FormsModule } from '@angular/forms';
import { ModalConfirmacionComponent } from './modal-confirmacion/modal-confirmacion.component';
import { ModalEmailConfirmComponent } from './modal-email-confirm/modal-email-confirm.component'; 
import { MatDialogModule } from '@angular/material/dialog';
import { CommonComponentsModule } from '../../@common-components/common-components.module';

@NgModule({
  declarations: [
    AdministratorComponent,
    ModalConfirmacionComponent,
    ModalEmailConfirmComponent
  ],
  imports: [
    CommonModule,
    AdministratorRoutingModule,
    CommonComponentsModule,
    NbAutocompleteModule,
    NbButtonModule,
    NbCardModule,
    NbFormFieldModule,
    NbIconModule,
    NbInputModule,
    NbLayoutModule,
    NbOptionModule,
    NbSelectModule,
    NbSidebarModule,
    RouterModule,
    NbThemeModule,
    NbPopoverModule,
    FormsModule,
    ReactiveFormsModule,
    NbDatepickerModule,
    MatDialogModule,
  ],
})
export class AdministratorModule {}

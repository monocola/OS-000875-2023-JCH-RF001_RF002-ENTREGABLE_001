import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { CuentasAsociadasRoutingModule } from './cuentas-asociadas-routing.module';
import { ModalReasignarComponent } from './modal-reasignar/modal-reasignar.component';
import { ModalRolesComponent } from './modal-roles/modal-roles.component';
import {
  NbActionsModule,
  NbAutocompleteModule,
  NbButtonModule,
  NbCardModule,
  NbDatepickerModule,
  NbFormFieldModule,
  NbIconModule,
  NbInputModule,
  NbSelectModule,
  NbTabsetModule,
  NbToggleModule,
  NbTooltipModule,
  NbTreeGridModule,
} from '@nebular/theme';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MatChipsModule } from '@angular/material/chips';
import { MatButtonModule } from '@angular/material/button';
import { ScrollingModule } from '@angular/cdk/scrolling';
import { MatDialogModule } from '@angular/material/dialog';
import { MatPaginatorModule } from '@angular/material/paginator';
import { MatDividerModule } from '@angular/material/divider';
import { CuentasAsociadasComponent } from './cuentas-asociadas.component';
import { CommonComponentsModule } from '../../@common-components/common-components.module';

@NgModule({
  imports: [
    CommonModule,
    CuentasAsociadasRoutingModule,
    CommonComponentsModule,
    NbFormFieldModule,
    NbInputModule,
    FormsModule,
    NbSelectModule,
    NbDatepickerModule,
    NbAutocompleteModule,
    NbTooltipModule,
    ReactiveFormsModule,
    MatChipsModule,
    NbTreeGridModule,
    NbCardModule,
    MatDividerModule,
    NbToggleModule,
    MatPaginatorModule,
    NbActionsModule,
    NbTabsetModule,
    MatDialogModule,
    ScrollingModule,
    MatButtonModule,
    NbIconModule,
    NbButtonModule,
  ],
  declarations: [
    ModalReasignarComponent,
    ModalRolesComponent,
    CuentasAsociadasComponent,
  ],
})
export class CuentasAsociadasModule {}

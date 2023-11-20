import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { GestoresOrhComponent } from './gestores-orh.component';
import { GestoresOrhRoutingModule } from './gestores-orh-routing.module';
import { ThemeModule } from '../../@theme/theme.module';

import {
  NbActionsModule,
  NbAutocompleteModule,
  NbButtonModule,
  NbCardModule,
  NbDatepickerModule,
  NbFormFieldModule,
  NbIconModule,
  NbInputModule,
  NbLayoutModule,
  NbPopoverModule,
  NbSelectModule,
  NbTabsetModule,
  NbToggleModule,
  NbTreeGridModule,
} from '@nebular/theme';

import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { CommonComponentsModule } from '../../@common-components/common-components.module';
import { MatDividerModule } from '@angular/material/divider';

import { NgxTrimDirectiveModule } from 'ngx-trim-directive';
import { MatTabsModule } from '@angular/material/tabs';
import { MatButtonModule } from '@angular/material/button';
import { MatPaginatorModule } from '@angular/material/paginator';
import { MatChipsModule } from '@angular/material/chips';
import { MatIconModule } from '@angular/material/icon';
import { MatAutocompleteModule } from '@angular/material/autocomplete';

import { MatTableModule } from '@angular/material/table';
import { MatSortModule } from '@angular/material/sort';
import { MatRippleModule } from '@angular/material/core';
import { MatCardModule } from '@angular/material/card';
import { NgxDropzoneModule } from 'ngx-dropzone';
import { MatDialogModule } from '@angular/material/dialog';
import { MatFormFieldModule } from '@angular/material/form-field';
import { ModalEditarOrhComponent } from './modal-editar-orh/modal-editar-orh.component';
import { ModalQuestionComponent } from './modal-question/modal-question.component';
import { ModalEliminarQuestionComponent } from './modal-eliminar-question/modal-eliminar-question.component';

@NgModule({
  declarations: [
    GestoresOrhComponent,
    ModalQuestionComponent,
    ModalEditarOrhComponent,
    ModalEliminarQuestionComponent],
  imports: [
    CommonModule,
    GestoresOrhRoutingModule,
    ThemeModule,
    NbButtonModule,
    FormsModule,
    ReactiveFormsModule,
    NbIconModule,
    CommonComponentsModule,
    NbLayoutModule,
    MatDividerModule,
    NbSelectModule,
    NbInputModule,
    ReactiveFormsModule,
    NbFormFieldModule,
    NbAutocompleteModule,
    NgxTrimDirectiveModule,
    MatTabsModule,
    NbPopoverModule,
    MatButtonModule,
    MatPaginatorModule,
    NbTreeGridModule,
    MatChipsModule,
    MatIconModule,
    MatAutocompleteModule,
    NbCardModule,
    NbTabsetModule,
    NbActionsModule,
    NbDatepickerModule,
    MatTableModule,
    MatSortModule,
    MatRippleModule,
    NbCardModule,
    NbTabsetModule,
    MatCardModule,
    NgxDropzoneModule,
    MatDialogModule,
    MatFormFieldModule,
    NbToggleModule
  ]
})
export class GestoresOrhModule { }

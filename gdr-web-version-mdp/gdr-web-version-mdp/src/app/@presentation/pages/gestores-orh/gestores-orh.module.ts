import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ThemeModule } from '../../@theme/theme.module';

import {
  NbActionsModule,
  NbAutocompleteModule,
  NbButtonModule, NbCardModule, NbDatepickerModule,
  NbFormFieldModule,
  NbIconModule,
  NbInputModule,
  NbLayoutModule, NbPopoverModule,
  NbSelectModule, NbTabsetModule, NbToggleModule, NbTreeGridModule
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
import { ModalQuestionComponent } from './modal-question/modal-question.component';

@NgModule({
  declarations: [
    ModalQuestionComponent],
  imports: [
    CommonModule,
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

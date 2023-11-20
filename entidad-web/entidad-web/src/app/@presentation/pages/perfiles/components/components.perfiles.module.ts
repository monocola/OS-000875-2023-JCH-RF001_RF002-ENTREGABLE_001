import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { BtnAddComponent } from './btn-add/btn-add.component';
import { ModalNivelEducatvoComponent } from './modal-nivel-educatvo/modal-nivel-educatvo.component';
import {
  NbButtonModule,
  NbFormFieldModule,
  NbIconModule,
  NbInputModule,
  NbSelectModule,
  NbTreeGridModule,
} from '@nebular/theme';
import { MatSelectModule } from '@angular/material/select';
import { MatTableModule } from '@angular/material/table';
import { MatRippleModule } from '@angular/material/core';
import { MatSortModule } from '@angular/material/sort';
import { MatPaginatorModule } from '@angular/material/paginator';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MatRadioModule } from '@angular/material/radio';
import { MatChipsModule } from '@angular/material/chips';
import { StringsArrayFieldComponent } from './strings-array-field/strings-array-field.component';
import { StringsLevelArrayFieldComponent } from './strings-level-array-field/strings-level-array-field.component';
import { StringsHoursArrayFieldComponent } from './strings-hours-array-field/strings-hours-array-field.component';
import { MonthAndYearFieldComponent } from './month-and-year-field/month-and-year-field.component';
import { ModalPreventCloseComponent } from './modal-prevent-close/modal-prevent-close.component';
import { TableLevelsComponent } from './table-levels/table-levels.component';
import { TableFormacionAcademicaComponent } from './table-formacion-academica/table-formacion-academica.component';
import { DragDropModule } from '@angular/cdk/drag-drop';
import { MatButtonModule } from '@angular/material/button';
import { BottomDivComponent } from './bottom-div/bottom-div.component';
import { RouterModule } from '@angular/router';
import { CommonComponentsModule } from 'src/app/@presentation/@common-components/common-components.module';

const COMPONENTS = [
  BtnAddComponent,
  ModalNivelEducatvoComponent,
  StringsArrayFieldComponent,
  StringsLevelArrayFieldComponent,
  StringsHoursArrayFieldComponent,
  MonthAndYearFieldComponent,
  ModalPreventCloseComponent,
  TableLevelsComponent,
  TableFormacionAcademicaComponent,
  BottomDivComponent,
];

@NgModule({
  imports: [
    CommonModule,
    NbFormFieldModule,
    ReactiveFormsModule,
    CommonComponentsModule,
    FormsModule,
    NbSelectModule,
    MatRadioModule,
    NbButtonModule,
    MatChipsModule,
    NbIconModule,
    NbInputModule,
    NbTreeGridModule,
    DragDropModule,
    MatButtonModule,
    RouterModule,
    MatSelectModule,
    MatTableModule,
    MatRippleModule,
    MatSortModule,
    MatPaginatorModule
  ],
  declarations: [...COMPONENTS],
  exports: [...COMPONENTS],
})
export class ComponentsPerfilesModule {}

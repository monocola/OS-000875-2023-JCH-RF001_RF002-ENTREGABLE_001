import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { ReporteServirRoutingModule } from './reporte-servir-routing.module';
import { ReporteServirComponent } from './reporte-servir.component';

import { MatDividerModule } from '@angular/material/divider';
import { MatStepperModule } from '@angular/material/stepper';
import { MatSlideToggleModule } from '@angular/material/slide-toggle';
import { MatDialogModule } from '@angular/material/dialog';
import { MatIconModule } from '@angular/material/icon';
import { MatTabsModule } from '@angular/material/tabs';

import {
  NbButtonModule,
  NbDatepickerModule,
  NbFormFieldModule,
  NbIconModule,
  NbInputModule,
  NbCardModule,
  NbPopoverModule,
  NbSelectModule,
  NbAutocompleteModule,
  NbRadioModule,
  NbCheckboxModule,
  NbTabsetModule,
} from '@nebular/theme';

import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { CommonComponentsModule } from '../../@common-components/common-components.module';

import { MatSelectModule } from '@angular/material/select';
import { MatTableModule } from '@angular/material/table';
import { MatRippleModule } from '@angular/material/core';
import { MatSortModule } from '@angular/material/sort';
import { MatPaginatorModule } from '@angular/material/paginator';
import { MatButtonModule } from '@angular/material/button';
import { TableReporteServirComponent } from './table-reporte-servir/table-reporte-servir.component';

@NgModule({
  declarations: [ReporteServirComponent, TableReporteServirComponent],
  imports: [
    CommonModule,
    ReporteServirRoutingModule,
    MatStepperModule,
    MatDialogModule,
    MatIconModule,
    FormsModule,
    ReactiveFormsModule,
    CommonComponentsModule,
    NbButtonModule,
    NbDatepickerModule,
    NbFormFieldModule,
    NbIconModule,
    NbInputModule,
    NbCardModule,
    NbPopoverModule,
    NbSelectModule,
    NbAutocompleteModule,
    NbRadioModule,
    MatSelectModule,
    NbCheckboxModule,
    NbTabsetModule,

    MatSlideToggleModule,
    MatButtonModule,

    MatDividerModule,
    MatPaginatorModule,
    MatTableModule,
    MatSortModule,
    MatRippleModule,
    MatTabsModule,
  ],
})

export class ReporteServirModule {}

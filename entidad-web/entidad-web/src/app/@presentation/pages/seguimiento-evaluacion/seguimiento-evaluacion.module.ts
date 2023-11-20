import { TableEvaluacionComponent } from './table-evaluacion/table-evaluacion.component';
import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { SeguimientoEvaluacionComponent } from './seguimiento-evaluacion.component';
import { SeguimientoEvaluacionRoutingModule } from './seguimiento-evaluacion-routing.module';
import { MatButtonModule } from '@angular/material/button';
import {
  NbButtonModule,
  NbDatepickerModule,
  NbFormFieldModule,
  NbIconModule,
  NbInputModule, NbPopoverModule,
  NbSelectModule
} from '@nebular/theme';
import { MatDividerModule } from '@angular/material/divider';
import { MatPaginatorModule } from '@angular/material/paginator';
import { MatTableModule } from '@angular/material/table';
import { MatSortModule } from '@angular/material/sort';
import { MatRippleModule } from '@angular/material/core';
import { CommonComponentsModule } from '../../@common-components/common-components.module';
import { MatSlideToggleModule } from '@angular/material/slide-toggle';
import { MatProgressBarModule } from '@angular/material/progress-bar';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';


@NgModule({
  declarations: [
    SeguimientoEvaluacionComponent,
    TableEvaluacionComponent,
  ],
  imports: [
    CommonModule,
    SeguimientoEvaluacionRoutingModule,
    MatButtonModule,
    MatProgressBarModule,
    NbIconModule,
    NbFormFieldModule,
    NbButtonModule,
    MatDividerModule,
    ReactiveFormsModule,
    NbSelectModule,
    NbInputModule,
    NbDatepickerModule,
    NbPopoverModule,
    MatPaginatorModule,
    MatTableModule,
    MatSortModule,
    MatRippleModule,
    CommonComponentsModule,
    MatSlideToggleModule,
    FormsModule,
  ]
})
export class SeguimientoEvaluacionModule {}

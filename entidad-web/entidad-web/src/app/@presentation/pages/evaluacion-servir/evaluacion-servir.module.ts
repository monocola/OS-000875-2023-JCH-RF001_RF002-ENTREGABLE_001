import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { EvaluacionServirRoutingModule } from './evaluacion-servir-routing.module';
import { EvaluacionServirComponent } from './evaluacion-servir.component';
import { MatDividerModule } from '@angular/material/divider';
import {
  NbButtonModule,
  NbFormFieldModule,
  NbIconModule,
  NbPopoverModule,
  NbSelectModule,
  NbToggleModule,
  NbTreeGridModule,
} from '@nebular/theme';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { TableEvaluacionServirComponent } from './table-evaluacion-servir/table-evaluacion-servir.component';
import { MatButtonModule } from '@angular/material/button';
import { ConfiguracionEvaluacionComponent } from './configuracion-evaluacion/configuracion-evaluacion.component';
import { DragDropModule } from '@angular/cdk/drag-drop';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { CommonComponentsModule } from '../../@common-components/common-components.module';

@NgModule({
  declarations: [
    EvaluacionServirComponent,
    TableEvaluacionServirComponent,
    ConfiguracionEvaluacionComponent,
  ],
  imports: [
    CommonModule,
    EvaluacionServirRoutingModule,
    MatDividerModule,
    NbIconModule,
    ReactiveFormsModule,
    NbPopoverModule,
    CommonComponentsModule,
    NbButtonModule,
    NbFormFieldModule,
    NbSelectModule,
    NbIconModule,
    NbTreeGridModule,
    NbButtonModule,
    NbToggleModule,
    NbButtonModule,
    MatButtonModule,
    DragDropModule,
    FormsModule,
    MatFormFieldModule,
    MatInputModule
  ],
  exports: [TableEvaluacionServirComponent],
})
export class EvaluacionServirModule {}

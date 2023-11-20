import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { EvaluacionEntidadRoutingModule } from './evaluacion-entidad-routing.module';
import { EvaluacionEntidadComponent } from './evaluacion-entidad.component';
import { MatDividerModule } from '@angular/material/divider';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { CommonComponentsModule } from '../../@common-components/common-components.module';
import {
  NbButtonModule,
  NbFormFieldModule,
  NbIconModule,
  NbSelectModule,
  NbTreeGridModule,
} from '@nebular/theme';
import { ModalJerarquiaComponent } from './modal-jerarquia/modal-jerarquia.component';
import { EvaluacionServirModule } from '../evaluacion-servir/evaluacion-servir.module';
import { MatButtonModule } from '@angular/material/button';
import { AdministrarEvaluacionesComponent } from './administrar-evaluaciones/administrar-evaluaciones.component';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { DragDropModule } from '@angular/cdk/drag-drop';
import { BottomDivEntidadComponent } from './bottom-div-entidad/bottom-div-entidad.component';
import { AngularResizedEventModule } from 'angular-resize-event';
import { MatIconModule } from '@angular/material/icon';
import { ModalInfoEvaluacionesComponent } from './modal-info-evaluaciones/modal-info-evaluaciones.component';

@NgModule({
  declarations: [
    EvaluacionEntidadComponent,
    ModalJerarquiaComponent,
    AdministrarEvaluacionesComponent,
    BottomDivEntidadComponent,
    ModalInfoEvaluacionesComponent,
  ],
  imports: [
    CommonModule,
    CommonComponentsModule,
    EvaluacionEntidadRoutingModule,
    MatDividerModule,
    ReactiveFormsModule,
    FormsModule,
    NbButtonModule,
    NbIconModule,
    EvaluacionServirModule,
    NbTreeGridModule,
    MatButtonModule,
    MatFormFieldModule,
    MatInputModule,
    DragDropModule,
    NbFormFieldModule,
    NbSelectModule,
    AngularResizedEventModule,
    MatButtonModule,
    MatIconModule,
  ],
})
export class EvaluacionEntidadModule {}

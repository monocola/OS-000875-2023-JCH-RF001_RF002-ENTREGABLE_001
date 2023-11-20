import { MatDialogModule } from '@angular/material/dialog';
import { MatIconModule } from '@angular/material/icon';
import { MatRadioModule } from '@angular/material/radio';
import { MatButtonModule } from '@angular/material/button';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatStepperModule } from '@angular/material/stepper';
import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { EvaluacionConfiguracionRoutingModule } from './evaluacion-configuracion-routing.module';
import { EvaluacionConfiguracionComponent } from './evaluacion-configuracion.component';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MatRippleModule } from '@angular/material/core';
import { MatDividerModule } from '@angular/material/divider';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import {
  NbActionsModule,
  NbButtonModule,
  NbCardModule,
  NbCheckboxModule,
  NbDatepickerModule,
  NbFormFieldModule,
  NbIconModule,
  NbInputModule,
  NbSelectModule,
} from '@nebular/theme';
import { CommonComponentsModule } from '../../@common-components/common-components.module';
import { ConfiguracionRequisitosMinComponent } from './configuracion-requisitos-min/configuracion-requisitos-min.component';
import { DialogGuardarConfiguracionPerfilComponent } from './dialog-guardar-configuracion-perfil/dialog-guardar-configuracion-perfil.component';
import { ModalElegirPlantillaComponent } from './modal-elegir-plantilla/modal-elegir-plantilla.component';

@NgModule({
  declarations: [EvaluacionConfiguracionComponent, ConfiguracionRequisitosMinComponent, DialogGuardarConfiguracionPerfilComponent, ModalElegirPlantillaComponent],
  imports: [
    CommonModule,
    EvaluacionConfiguracionRoutingModule,
    MatDividerModule,
    MatRippleModule,
    NbButtonModule,
    CommonComponentsModule,
    MatFormFieldModule,
    MatInputModule,
    FormsModule,
    ReactiveFormsModule,
    NbIconModule,
    NbInputModule,
    NbFormFieldModule,
    NbDatepickerModule,
    MatStepperModule,
    MatCheckboxModule,
    NbCardModule,
    NbSelectModule,
    NbCheckboxModule,
    MatButtonModule,
    MatRadioModule,
    MatIconModule,
    NbActionsModule,
    MatDialogModule,
  ],
})
export class EvaluacionConfiguracionModule {}

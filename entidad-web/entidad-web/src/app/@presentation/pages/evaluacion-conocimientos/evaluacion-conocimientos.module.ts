import { MatTabsModule } from '@angular/material/tabs';
import { MatDialogModule } from '@angular/material/dialog';
import { CommonComponentsModule } from './../../@common-components/common-components.module';
import { MatRippleModule } from '@angular/material/core';
import { NgModule, CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { CommonModule, CurrencyPipe } from '@angular/common';
import { EvaluacionConocimientosComponent } from './evaluacion-conocimientos.component';
import { EvaluacionConocimientosRoutingModule } from './evaluacion-conocimientos-routing.module';
import { MatDividerModule } from '@angular/material/divider';
import { ExamenesComponent } from './examenes/examenes.component';
import { CategoriaComponent } from './categoria/categoria.component';
import { CategoriaAddComponent } from './categoria/add/addcategoria.component';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { EvaluacionConService } from './evaluacion-conocimientos.service';
import { RegistroComponent } from './examenes/registro/registro.component';
import { ModalCrearExamenComponent } from './examenes/modal-examen/modal-examen.component';
import { ProgramarComponent } from './programar/programar.component';
import { NuevoGrupoComponent } from './programar/nuevo-grupo/nuevogrupo-component';
import { RegistroMasivoComponent } from './categoria/registro-masivo/registro-masivo.component';
import { ProgEvaluacionComponent } from './programar/crear/progeva-component';
import { ExamenVirtualComponent } from './programar/crear/examen-virtual/examen-virtual/examen-virtual.component';
import { ExamenVirtualCabeceraComponent } from './programar/crear/examen-virtual/examen-virtual-cabecera/examen-virtual-cabecera.component';
import { MatCardModule } from '@angular/material/card';
import { ModalCrearGrupoComponent } from './programar/modal-crear-grupo/modal-crear-grupo.component';
import {
  NgxMatDatetimePickerModule,
  NgxMatTimepickerModule,
  NgxMatNativeDateModule,
} from '@angular-material-components/datetime-picker';
import { MatButtonModule } from '@angular/material/button';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatDatepickerModule } from '@angular/material/datepicker';
import { MatRadioModule } from '@angular/material/radio';
import { MatSelectModule } from '@angular/material/select';
import { ExamenVirtualResultadosComponent } from './programar/crear/examen-virtual/examen-virtual-resultados/examen-virtual-resultados.component';
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
} from '@nebular/theme';
import { MatListModule } from '@angular/material/list';
import { MatCarouselModule } from '@ngmodule/material-carousel';
import { ModalCrearPreguntaComponent } from './preguntas/modal-pregunta.component';
import { ListaEvaluacionesModule } from '../lista-evaluaciones/lista-evaluaciones.module';
import { StrictNumberOnlyDirectiveModule } from '../../@common-components/onlyNumber/StrictNumberOnlyDirective';

import { ModalSubirPreguntasMasivasComponent } from './examenes/modal-subir-preguntas-masivas/modal-subir-preguntas-masivas.component';
import { EditarPreguntasComponent } from './examenes/editar-preguntas/editar-preguntas.component';
import { VisualizarExamenComponent } from './examenes/visualizar-examen/visualizar-examen.component';
import { MatIconModule } from '@angular/material/icon';
@NgModule({
  declarations: [
    EvaluacionConocimientosComponent,
    ExamenesComponent,
    CategoriaComponent,
    CategoriaAddComponent,
    ModalCrearPreguntaComponent,
    RegistroComponent,
    ModalCrearExamenComponent,
    ProgramarComponent,
    NuevoGrupoComponent,
    ProgEvaluacionComponent,
    RegistroMasivoComponent,
    ExamenVirtualComponent,
    ExamenVirtualCabeceraComponent,
    ModalCrearGrupoComponent,
    ExamenVirtualResultadosComponent,
    ModalSubirPreguntasMasivasComponent,
    EditarPreguntasComponent,
    VisualizarExamenComponent,
  ],
  imports: [
    CommonModule,
    EvaluacionConocimientosRoutingModule,
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
    NbCardModule,
    NbDatepickerModule,
    NgxMatDatetimePickerModule,
    NgxMatTimepickerModule,
    NgxMatNativeDateModule,
    MatButtonModule,
    MatCheckboxModule,
    MatDatepickerModule,
    MatRadioModule,
    MatSelectModule,
    MatCardModule,
    NbFormFieldModule,
    NbPopoverModule,
    NbSelectModule,
    NbAutocompleteModule,
    MatDialogModule,
    MatTabsModule,
    MatListModule,
    MatCarouselModule,
    NbRadioModule,
    ListaEvaluacionesModule,
    StrictNumberOnlyDirectiveModule,
    MatIconModule

  ],
  providers: [EvaluacionConService, CurrencyPipe],
  schemas: [CUSTOM_ELEMENTS_SCHEMA],
})
export class EvaluacionConocimientosModule { }

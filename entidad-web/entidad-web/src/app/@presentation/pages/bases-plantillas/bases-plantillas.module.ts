import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { BasesPlantillasRoutingModule } from './bases-plantillas-routing.module';
import { BasesPlantillasComponent } from './bases-plantillas.component';
import { NbButtonModule, NbFormFieldModule } from '@nebular/theme';
import { MatDividerModule } from '@angular/material/divider';
import { CommonComponentsModule } from '../../@common-components/common-components.module';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { CreacionPlantillasComponent } from './creacion-plantillas/creacion-plantillas.component';
import { RouterModule } from '@angular/router';

@NgModule({
  declarations: [BasesPlantillasComponent, CreacionPlantillasComponent],
  imports: [
    CommonModule,
    BasesPlantillasRoutingModule,
    NbButtonModule,
    MatDividerModule,
    CommonComponentsModule,
    NbFormFieldModule,
    ReactiveFormsModule,
    FormsModule,
    RouterModule,
  ],
})
export class BasesPlantillasModule {}

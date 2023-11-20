import {
  ModuleWithProviders,
  NgModule,
  Optional,
  SkipSelf,
} from '@angular/core';
import { CommonModule } from '@angular/common';
import {
  AdministratorService,
  AuthenticationService,
  ParameterService,
  SunatService,
} from '../@data/services';
import { throwIfAlreadyLoaded } from './module-import-guard';
import { AuthenticationRepository } from './repository/authentication.repository';
import { ParameterRepository } from './repository/parameter.repository';
import { SunatRepository } from './repository/sunat.repository';
import { AdministratorRepository } from './repository/administrator.repository';

import { ReniecRepository } from './repository/reniec.repository';
import { ReniecService } from '../@data/services/reniec.service';
import { ReunionesRepository } from './repository/reuniones.repository';
import { ReunionesService } from '../@data/services/reuniones.service';

import { PerfilesRepository } from './repository/perfiles.repository';
import { PerfilesService } from 'src/app/@data/services/perfiles.service';

import { BannerRepository } from './repository/banner.repository';
import { BannerService } from '../@data/services/banner.service';
import { MaestraParametroRepository } from './repository/maestra-parametro.repository';
import { MaestraParametroService } from '../@data/services/maestra-parametro.service';

import { ServidoresRepository } from './repository/servidores.repository';
import { ServidoresService } from 'src/app/@data/services/servidores.service';

import { OrganigramaRepository } from './repository/organigrama.repository';
import { OrganigramaService } from 'src/app/@data/services/organigrama.service';

import { OrganoRepository } from './repository/organo.repository';
import { OrganoService } from 'src/app/@data/services/organo.service';

import { UnidadOrganicaService } from 'src/app/@data/services/unidad-organica.service';
import { UnidadOrganicaRepository } from './repository/unidad-organica.repository';


import { CronogramaService } from 'src/app/@data/services/cronograma.service';
import { CronogramaRepository } from './repository/cronograma.repository';
import { UtilService } from '../@data/services/util.service';
import { UtilRepository } from './repository/util.repository';
import { MetaRepository } from './repository/meta.repository';
import { MetaService } from '../@data/services/meta.service';
import { CicloRepository } from './repository/ciclo.repository';
import { CicloService } from '../@data/services/ciclo.service';
import { ImplementacionRepository } from './repository/implementacion.repository';
import { ImplementacionService } from '../@data/services/implementacion.service';
import { ConfiguracionRepository } from './repository/configuracion.repository';
import { ConfiguracionService } from '../@data/services/configuracion.service';
import { PermissionRepository } from './repository/permission.repository';
import { PermissionService } from '../@data/services/permission.service';






const DATA_SERVICES = [
  {
    provide: AuthenticationRepository,
    useClass: AuthenticationService,
  },
  { provide: ParameterRepository, useClass: ParameterService },
  { provide: SunatRepository, useClass: SunatService },

  { provide: AdministratorRepository, useClass: AdministratorService },
  { provide: ReniecRepository, useClass: ReniecService },
  { provide: ReunionesRepository, useClass: ReunionesService },

  { provide: PerfilesRepository, useClass: PerfilesService },

  { provide: BannerRepository, useClass: BannerService },

  { provide: MaestraParametroRepository, useClass: MaestraParametroService},

  { provide: ServidoresRepository, useClass: ServidoresService },
  { provide: OrganigramaRepository, useClass: OrganigramaService },

  { provide: OrganoRepository, useClass: OrganoService },


  {
    provide: UnidadOrganicaRepository,
    useClass: UnidadOrganicaService,

  },

  { provide: CronogramaRepository, useClass: CronogramaService },

  { provide: UtilRepository, useClass: UtilService },

  { provide: MetaRepository, useClass: MetaService },

  { provide: CicloRepository, useClass: CicloService },
  { provide: ImplementacionRepository, useClass: ImplementacionService },
  { provide: ConfiguracionRepository, useClass: ConfiguracionService },
  { provide: PermissionRepository, useClass: PermissionService },

];

@NgModule({
  declarations: [],
  imports: [CommonModule],
})
export class DomainModule {
  constructor(@Optional() @SkipSelf() parentModule: DomainModule) {
    throwIfAlreadyLoaded(parentModule, 'CoreModule');
  }

  static forRoot(): ModuleWithProviders<any> {
    return {
      ngModule: DomainModule,
      providers: [...DATA_SERVICES],
    } as ModuleWithProviders<any>;
  }
}

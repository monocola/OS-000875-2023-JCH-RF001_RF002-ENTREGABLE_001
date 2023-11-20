export class ItemMenu {
    descripcion: string;
    icono: string;
    url: string;
    subMenu?: ItemMenu[];
    opened?: boolean;
    childSelected?: boolean;
}

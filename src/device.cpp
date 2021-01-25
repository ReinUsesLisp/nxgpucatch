#include <deko3d.hpp>

dk::UniqueDevice device;
dk::UniqueQueue queue;

void InitializeDevice() {
    device = dk::DeviceMaker{}
                 .setFlags(DkDeviceFlags_DepthMinusOneToOne | DkDeviceFlags_OriginUpperLeft)
                 .create();
    queue = dk::QueueMaker{device}.create();
}